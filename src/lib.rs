//! Stock code for writing APDUs that use the block protocol using futures.
//!
//!
//! To use, for each APDU write a struct type for each APDU and provide instances of [AsyncAPDU]
//! and [AsyncAPDUStated], and call [poll_apdu_handler] when dispatch identifies that APDU. The
//! future returned by [AsyncAPDU::run] can use any parser defined via
//! [ledger_parser_combinators::async_parser] by simply passing the appropriate [ByteStream] to it
//! and using `.await`.
//!
//! In order to implement [AsyncAPDUStated] it will be necessary to write a StateHolder enum, and
//! provide an empty struct implementing StateHolderCtr to AsyncAPDUStated; the StateHolderCtr is
//! essentially a constructor for a StateHolder with the appropriate lifetime variable;
//! [AsyncAPDUStated::init] should call the [AsyncAPDU::run] and put the result in an appropriate
//! variant of the StateHolder enum, and [AsyncAPDUStated::poll] should match on the StateHolder
//! enum and poll the contained future if the APDU is the current one.
//!
//! From within [AsyncAPDU::run] there is access to both a [ByteStream] for each parameter of the
//! APDU supporting [ledger_parser_combinators::async_parser::Readable] and a handle to [HostIO];
//! [HostIO::get_chunk] and [HostIO::put_chunk] may be used to read and write arbitrary chunks of
//! content-indexed data from and to the host, and [HostIO::result_accumulating] and
//! [HostIO::result_final] send pieces of final result to the host. An APDU handler must call
//! [HostIO::result_final] upon successful completion of the APDU.
//!
//! # Example
//!
//!
//! ```
//! use core::future::Future;
//! use core::task::*;
//! use ledger_async_block::*;
//! use ledger_parser_combinators::async_parser::*;
//! use pin_project::pin_project;
//!
//!
//! struct ExampleAPDU;
//!
//! impl AsyncAPDU for ExampleAPDU {
//!     type State<'a> = impl Future<Output = ()>;
//!     fn run<'a>(self, io: HostIO, input: ArrayVec<ByteStream, MAX_PARAMS>) -> Self::State<'a> {
//!         async move {
//!             // Parse an array of four bytes from the input; ignore any remainder.
//!             let data = (DefaultInterp as AsyncParser<Array<u8, 4>>).parse(&mut input[0]).await;
//!             // Send the four bytes back to the host.
//!             io.result_final(&data).await;
//!         }
//!     }
//! }
//!
//! // State holder for the APDUs
//! #[pin_project(project = APDUStateProjection)]
//! enum APDUState<'a> {
//!   NoState,
//!   ExampleAPDUState(#[pin] <ExampleAPDU as AsyncAPDU>::State<'a>),
//! }
//!
//! static mut COMM_CELL : Option<RefCell<io::Comm>> = None;
//! static mut HOST_IO_STATE : Option<RefCell<HostIOState>> = None;
//! static mut STATES_BACKING : APDUState<'static> = APDUState::NoState;
//!
//! unsafe fn initialize() {
//!     COMM_CELL=Some(RefCell::new(io::Comm::new()));
//!     let comm = COMM_CELL.as_ref().unwrap();
//!     HOST_IO_STATE = Some(RefCell::new(HostIOState {
//!         comm: comm,
//!         requested_block: None,
//!         sent_command: None,
//!     }));
//! }
//!
//!
//! extern "C" fn sample_main() {
//!   ...
//!   unsafe { initialize(); }
//!   let mut states = unsafe { Pin::new_unchecked( &mut STATES_BACKING ) };
//!   ...
//!   match handle_apdu(host_io, ins, &mut states) {
//!     Ok(()) => { comm.borrow_mut().reply_ok() }
//!     Err(sw) => { comm.borrow_mut().reply(sw) }
//!   }
//! }
//!
//! handle_apdu<'a: 'b, 'b>(io: HostIO, ins: Ins, state: &'b mut Pin<&'a mut ParsersState<'a>>)
//!     -> Result<(), Reply> {
//!         match ins {
//!             Ins::ExampleAPDU => poll_apdu_handler(state, io, ExampleAPDU)?
//!         }
//!     }
//!
//! // Boilerplate to set and retrieve states from the state holder; should really be simpler than
//! // this.
//! struct APDUStateConstructor;
//! impl StateHolderCtr for APDUStateConstructor {
//!     type StateCtr<'a> = APDUState<'a>;
//! }
//!
//! impl AsyncAPDUStated for ExampleAPDU {
//!     #[inline(never)]
//!     fn init<'a, 'b: 'a>(
//!         self,
//!         s: &mut core::pin::Pin<&'a mut ParsersState<'a>>,
//!         io: HostIO,
//!         input: ArrayVec<ByteStream, MAX_PARAMS>
//!     ) -> () {
//!         s.set(APDUState::ExampleAPDUState(self.run(io, input)));
//!     }
//!     #[inline(never)]
//!     fn poll<'a, 'b>(self, s: &mut core::pin::Pin<&'a mut ParsersState>) -> core::task::Poll<()> {
//!         let waker = unsafe { Waker::from_raw(RawWaker::new(&(), &RAW_WAKER_VTABLE)) };
//!         let mut ctxd = Context::from_waker(&waker);
//!         match s.as_mut().project() {
//!             APDUStateProjection::ExampleAPDUState(ref mut s) => s.as_mut().poll(&mut ctxd),
//!             _ => panic!("Ooops"),
//!         }
//!     }
//! }
//!
//! ```
//!
#![no_std]
#![allow(incomplete_features)]
#![feature(type_alias_impl_trait)]

#![feature(cfg_version)]
#![cfg_attr(all(target_family="bolos", not(version("1.64"))), feature(future_poll_fn))]
#![cfg_attr(all(target_family="bolos", not(version("1.65"))), feature(generic_associated_types))]

use ledger_log::*;
use nanos_sdk::io;
use arrayvec::ArrayVec;
use core::future::Future;
use nanos_sdk::bindings::*;
use ledger_parser_combinators::async_parser::{Readable, UnwrappableReadable, reject};

use nanos_sdk::io::Reply;
use core::convert::TryFrom;
use core::convert::TryInto;
use core::task::*;
use core::cell::{RefCell, Ref, RefMut}; //, BorrowMutError};


#[repr(u8)]
#[derive(Debug)]
enum HostToLedgerCmd {
    START = 0,
    GetChunkResponseSuccess = 1,
    GetChunkResponseFailure = 2,
    PutChunkResponse = 3,
    ResultAccumulatingResponse = 4
}

impl TryFrom<u8> for HostToLedgerCmd {
    type Error = Reply;
    fn try_from(a: u8) -> Result<HostToLedgerCmd, Reply> {
        match a {
            0 => Ok(HostToLedgerCmd::START),
            1 => Ok(HostToLedgerCmd::GetChunkResponseSuccess),
            2 => Ok(HostToLedgerCmd::GetChunkResponseFailure),
            3 => Ok(HostToLedgerCmd::PutChunkResponse),
            4 => Ok(HostToLedgerCmd::ResultAccumulatingResponse),
            _ => Err(io::StatusWords::Unknown.into()),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LedgerToHostCmd {
    ResultAccumulating = 0, // Not used yet in this app.
    ResultFinal = 1,
    GetChunk = 2,
    PutChunk = 3
}

const HASH_LEN: usize = 32;
pub type SHA256 = [u8; HASH_LEN];


#[derive(Debug)]
pub struct ChunkNotFound;

pub struct HostIOState {
    pub comm: &'static RefCell<io::Comm>,
    pub requested_block: Option<SHA256>,
    pub sent_command: Option<LedgerToHostCmd>
}

impl core::fmt::Debug for HostIOState {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "HostIOState {{ comm: {}, requested_block: {:?}, sent_command: {:?} }}", if self.comm.try_borrow().is_ok() {"not borrowed"} else {"borrowed"}, self.requested_block, self.sent_command)
    }
}

#[derive(PartialEq,Debug)]
pub enum AsyncTrampolineResult {
    NothingPending,
    Pending,
    Resolved
}

pub trait AsyncTrampoline {
    fn handle_command(&mut self) -> AsyncTrampolineResult;
}
impl AsyncTrampoline for () {
    fn handle_command(&mut self) -> AsyncTrampolineResult { AsyncTrampolineResult::NothingPending }
}


#[derive(Copy, Clone, Debug)]
pub struct HostIO(pub &'static RefCell<HostIOState>);

impl HostIO {

    /// Grab a mutable reference to the [io::Comm] object, to do some direct stuff with.
    pub fn get_comm<'a>(self) -> Result<RefMut<'a, io::Comm>, Reply> {
        self.0.try_borrow_mut().or(Err(io::StatusWords::Unknown))?.comm.try_borrow_mut().or(Err(io::StatusWords::Unknown.into()))
    }

    /// Get a chunk, identified by SHA256 hash of it's contents, from the host.
    pub fn get_chunk(self, sha: SHA256) -> impl Future<Output = Result<Ref<'static, [u8]>, ChunkNotFound>> {
        core::future::poll_fn(move |_| {
            match self.0.try_borrow_mut() {
                Ok(ref mut s) => {
                    if s.sent_command.is_some() {
                        Poll::Pending
                    } else {
                        if s.requested_block == Some(sha) {
                            match s.comm.borrow().get_data().ok().unwrap()[0].try_into() {
                                Ok(HostToLedgerCmd::GetChunkResponseSuccess) => {
                                    Poll::Ready(Ok(Ref::map(s.comm.borrow(), |comm| &comm.get_data().ok().unwrap()[1..])))
                                }
                                Ok(HostToLedgerCmd::GetChunkResponseFailure) => Poll::Ready(Err(ChunkNotFound)),
                                _ => {
                                    error!("Reached unreachable");
                                    panic!("Unreachable: should be filtered out by protocol rules before this point.")
                                }
                            }
                        } else {
                            s.requested_block = Some(sha);
                            s.sent_command = Some(LedgerToHostCmd::GetChunk);
                            let mut io = s.comm.borrow_mut();
                            io.append(&[LedgerToHostCmd::GetChunk as u8]);
                            io.append(&sha);
                            Poll::Pending
                        }
                    }
                }
                Err(_) => {
                    Poll::Pending
                }
            }
        })
    }

    fn send_write_command<'a: 'c, 'b: 'c, 'c>(self, cmd: LedgerToHostCmd, data: &'b [u8]) -> impl 'c + Future<Output = ()> {
        core::future::poll_fn(move |_| {
            match self.0.try_borrow_mut() {
                Ok(ref mut s) => {
                    if s.sent_command.is_some() {
                        Poll::Pending
                    } else {
                        s.requested_block = None;
                        s.sent_command = Some(cmd);
                        let mut io = s.comm.borrow_mut();
                        io.append(&[cmd as u8]);
                        io.append(data);
                        Poll::Pending
                    }
                }
                Err(_) => Poll::Pending,
            }
        })
    }

    /// Write a chunk to the host, returning the SHA256 of it's contents, used as an address for a
    /// future get_chunk.
    pub fn put_chunk<'a: 'c, 'b: 'c, 'c>(self, chunk: &'b [u8]) -> impl 'c + Future<Output = SHA256> {
        async move {
            self.send_write_command(LedgerToHostCmd::PutChunk, chunk).await;
            sha256_hash(chunk)
        }
    }

    /// Write a piece of output to the host, but don't declare that we are done; the host will
    /// return to the ledger to get more.
    pub fn result_accumulating<'a: 'c, 'b: 'c, 'c>(self, chunk: &'b [u8]) -> impl 'c + Future<Output = ()> {
        self.send_write_command(LedgerToHostCmd::ResultAccumulating, chunk)
    }
    /// Write the final piece of output to the host; after this, we're done and the host does not
    /// contact us again on this subject.
    pub fn result_final<'a: 'c, 'b: 'c, 'c>(self, chunk: &'b [u8]) -> impl 'c + Future<Output = ()> {
        self.send_write_command(LedgerToHostCmd::ResultFinal, chunk)
    }
}

/// Concrete implementation of the interface defined by
/// [ledger_parser_combinators::async_parser::Readable] for the block protocol.
#[derive(Clone)]
pub struct ByteStream {
    host_io: HostIO,
    current_chunk: SHA256,
    current_offset: usize
}

impl Readable for ByteStream {
    type OutFut<'a, const N: usize> = impl 'a + core::future::Future<Output = [u8; N]>;
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N> {
        async move {
            let mut buffer = ArrayVec::<u8, N>::new();
            while !buffer.is_full() {
                if self.current_chunk == [0; 32] {
                    let _ : () = reject().await;
                }
                let chunk_res = self.host_io.get_chunk(self.current_chunk).await;
                let chunk = match chunk_res { Ok(a) => a, Err(_) => reject().await, };
                let avail = &chunk[self.current_offset+HASH_LEN .. ];
                let consuming = core::cmp::min(avail.len(), buffer.remaining_capacity());
                buffer.try_extend_from_slice(&avail[0..consuming]).ok();
                self.current_offset += consuming;
                if self.current_offset + HASH_LEN == chunk.len() {
                    self.current_chunk = chunk[0..HASH_LEN].try_into().unwrap();
                    self.current_offset = 0;
                }
            }
            buffer.into_inner().unwrap()
        }
    }
}

impl UnwrappableReadable for ByteStream {
    type Wrapped = Self;
    fn unwrap_clone(&self) -> Self::Wrapped {
        self.clone()
    }
}

// We'd really rather have this be part of AsyncAPDU below, but the compiler crashes at the moment
// if we do that. If it stops crashing, delete this line and make everywhere that uses MAX_PARAMS
// refer to the relevant AsyncAPDU.
pub const MAX_PARAMS: usize = 2;

pub trait AsyncAPDU : 'static + Sized {
    // const MAX_PARAMS: usize;
    type State<'c>: Future<Output = ()>;
    // fn run<'c>(self, io: HostIO, input: ArrayVec<ByteStream, { Self::MAX_PARAMS }>) -> Self::State<'c>;
    fn run<'c>(self, io: HostIO, input: ArrayVec<ByteStream, MAX_PARAMS>) -> Self::State<'c>;
}

pub trait StateHolderCtr {
    type StateCtr<'a> : Default;
}

pub trait AsyncAPDUStated<StateHolderT: 'static + StateHolderCtr> : AsyncAPDU {
    fn init<'a, 'b: 'a>(
        self,
        s: &mut core::pin::Pin<&'a mut StateHolderT::StateCtr<'a>>,
        io: HostIO,
        input: ArrayVec<ByteStream, MAX_PARAMS>
    ) -> ();

    // fn get<'a, 'b>(self, s: &'b mut core::pin::Pin<&'a mut StateHolderT::StateCtr<'a>>) -> Option<&'b mut core::pin::Pin<&'a mut Self::State<'a>>>;

    fn poll<'a, 'b>(self, s: &'b mut core::pin::Pin<&'a mut StateHolderT::StateCtr<'a>>) -> core::task::Poll<()>;

    /* {
        let waker = unsafe { Waker::from_raw(RawWaker::new(&(), &RAW_WAKER_VTABLE)) };
        let mut ctxd = Context::from_waker(&waker);
        match self.get(s) {
            Some(ref mut s) => s.as_mut().poll(&mut ctxd),
            None => panic!("Oops"),
        }
    }*/
}

pub static RAW_WAKER_VTABLE : RawWakerVTable = RawWakerVTable::new(|a| RawWaker::new(a, &RAW_WAKER_VTABLE), |_| {}, |_| {}, |_| {});

/// Main entry point: run an AsyncAPDU given an input.
#[inline(never)]
pub fn poll_apdu_handler<'a: 'b, 'b, StateHolderT: 'static + StateHolderCtr, A: 'a + AsyncAPDUStated<StateHolderT> + Copy, T: AsyncTrampoline>     (
    s: &'b mut core::pin::Pin<&'a mut StateHolderT::StateCtr<'a>>,
    io: HostIO,
    trampoline: &mut T,
    apdu: A
) -> Result<(), Reply> where [(); MAX_PARAMS]: Sized {
    let command = io.get_comm()?.get_data()?[0].try_into();
    match command {
        Ok(HostToLedgerCmd::START) => {
            call_me_maybe( || {
            let mut params = ArrayVec::<ByteStream, MAX_PARAMS>::new();
            for param in io.get_comm().ok()?.get_data().ok()?[1..].chunks_exact(HASH_LEN) {
                params.try_push(ByteStream {
                    host_io: io,
                    current_chunk: param.try_into().or(Err(io::StatusWords::Unknown)).ok()?,
                    current_offset: 0
                }).ok()?;
            }
            apdu.init(s, io, params);
            Some(())
            } ).ok_or(io::StatusWords::Unknown)?;
        }
        Ok(HostToLedgerCmd::GetChunkResponseSuccess) if io.0.borrow().sent_command == Some(LedgerToHostCmd::GetChunk) => {
            if io.0.borrow().comm.borrow().get_data()?.len() < HASH_LEN+1 { return Err(io::StatusWords::Unknown.into()); }

            // Check the hash, so the host can't lie.
            call_me_maybe( || {
                let hashed = sha256_hash(&io.0.borrow().comm.borrow().get_data().ok()?[1..]);
                
                if Some(hashed) != io.0.borrow().requested_block {
                    None
                } else {
                    Some(())
                }
            }).ok_or(io::StatusWords::Unknown)?;
        }
        // Only need to check that these are responses to things we did; there's no data to
        // validate.
        Ok(HostToLedgerCmd::GetChunkResponseFailure) if io.0.borrow().sent_command == Some(LedgerToHostCmd::GetChunk) => { }
        Ok(HostToLedgerCmd::PutChunkResponse) if io.0.borrow().sent_command == Some(LedgerToHostCmd::PutChunk) => { }
        Ok(HostToLedgerCmd::ResultAccumulatingResponse) if io.0.borrow().sent_command == Some(LedgerToHostCmd::ResultAccumulating) => { }
        // Reject otherwise.
        _ => Err(io::StatusWords::Unknown)?,
    }

    
    // We use this to wait if we've already got a command to send, so clear it now that we're
    // in a validated state.
    io.0.borrow_mut().sent_command=None;

    loop {
        // And run the future for this APDU.
        match apdu.poll(s) {
            Poll::Pending => {
                let mut trampoline_res = AsyncTrampolineResult::Pending;
                // First, clear any trampolines we might have pending
                while trampoline_res == AsyncTrampolineResult::Pending {
                    trampoline_res = trampoline.handle_command();
                }
                // Then, check that if we're waiting that we've actually given the host something to do.
                if io.0.borrow().sent_command.is_some() {
                    return Ok(())
                } else if trampoline_res == AsyncTrampolineResult::Resolved {
                } else {
                    error!("APDU handler future neither completed nor sent a command; something is probably missing an .await");
                    Err(io::StatusWords::Unknown)?
                }
            }
            Poll::Ready(()) => {
                if io.0.borrow().sent_command.is_none() {
                    error!("APDU handler future completed but did not send a command; the last line is probably missing an .await");
                    Err(io::StatusWords::Unknown)?
                }
                s.set(core::default::Default::default());
                return Ok(())
            }
        }
    }
}

// Hashing required for validating blocks from the host.

fn sha256_hash(data: &[u8]) -> [u8; 32] {
    let mut rv = [0; 32];
    unsafe {
        let mut hasher = cx_sha256_s::default();
        cx_sha256_init_no_throw(&mut hasher);
        let hasher_ref = &mut hasher as *mut cx_sha256_s as *mut cx_hash_t;
        cx_hash_update(hasher_ref, data.as_ptr(), data.len() as u32);
        cx_hash_final(hasher_ref, rv.as_mut_ptr());
    }
    rv
}

// Stack control helper.
#[inline(never)]
pub fn call_me_maybe<F: FnOnce() -> Option<()>>(f: F) -> Option<()> {
    f()
}

