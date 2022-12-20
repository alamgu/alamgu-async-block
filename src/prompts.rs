use arrayvec::ArrayString;
use crate::*;
use core::fmt::{Arguments, Write, Error};

use nanos_ui::{bagls::*, ui::*};
use nanos_sdk::buttons::*;
use nanos_sdk::io::Event;
use ledger_log::*;

#[derive(Clone, Debug)]
pub struct PromptQueue {
    io: HostIO,
    prev: SHA256Sum,
}

const PROMPT_CHUNK_LENGTH : usize = 16;
type PromptBuffer = ArrayVec::<u8, {32+19+PROMPT_CHUNK_LENGTH}>;

#[derive(Debug, PartialEq)]
pub struct PromptingError;

impl From<core::str::Utf8Error> for PromptingError { fn from(_ : core::str::Utf8Error) -> PromptingError { PromptingError } }
impl From<arrayvec::CapacityError> for PromptingError { fn from(_ : arrayvec::CapacityError) -> PromptingError { PromptingError } }
impl From<arrayvec::CapacityError<&str>> for PromptingError { fn from(_ : arrayvec::CapacityError<&str>) -> PromptingError { PromptingError } }
impl From<ChunkNotFound> for PromptingError { fn from(_ : ChunkNotFound) -> PromptingError { PromptingError } }

impl PromptQueue {
    pub fn new(io: HostIO) -> PromptQueue { PromptQueue { io, prev: [0; 32] } }

    async fn pop(&mut self) -> Result<Option<(ArrayString<16>, ArrayString<PROMPT_CHUNK_LENGTH>)>, PromptingError> {
        if self.prev == [0; 32] {
            return Ok(None);
        }
        let chunk = PromptBuffer::try_from(self.io.get_chunk(self.prev).await?.as_ref())?;
        self.prev[0..32].copy_from_slice(&chunk[0..32]);
        let title = ArrayString::try_from(core::str::from_utf8(&chunk[34..34+chunk[32] as usize])?)?;
        let body = ArrayString::try_from(core::str::from_utf8(&chunk[34+chunk[32] as usize..(34+chunk[32]+chunk[33]) as usize])?)?;
        Ok(Some((title, body)))
    }

    async fn reverse(&self) -> Result<Self, PromptingError> {
        let mut cursor = self.clone();
        let mut accum = Self::new(self.io);
        while let Some((title, body)) = cursor.pop().await? {
            accum.add_prompt_chunk(&title, &body).await;
        }
        Ok(accum)
    }

    pub async fn show(&mut self) -> Result<bool, PromptingError> {
        trace!("Attempting to prompt, {:?}", self);
        if self.prev == [0; 32] { return Err(PromptingError); } // No showing empty PromptQueues.
        
        let mut forward = self.reverse().await?;
        trace!("Reversed");
        let mut backward = Self::new(self.io);
        trace!("Backward crated");
        let mut title_and_body = forward.pop().await?.expect("already checked for this?");
        trace!("Got first page, {}, {}", title_and_body.0, title_and_body.1);

        #[derive(Clone, Copy, Debug)]
        enum PromptingState {
            Prompts,
            Confirm,
            Cancel
        }
        let mut state = PromptingState::Prompts;
        let mut buttons = Default::default();

        loop {
            // Display
            trace!("Looping on prompts");
            let (currentTitle, currentBody) = title_and_body;
            match state {
                PromptingState::Prompts => {
                    trace!("Showing prompts");
                    Bagl::LABELLINE(LabelLine::new().pos(0, 10).text(currentTitle.as_str())).display();
                    Bagl::LABELLINE(LabelLine::new().pos(0, 25).text(currentBody.as_str())).paint();
                    if backward.prev != [0; 32] {
                        RIGHT_ARROW.paint();
                    }
                    LEFT_ARROW.paint();
                }
                PromptingState::Confirm => {
                    trace!("Showing confirm");
                    Bagl::ICON(Icon::new(Icons::CheckBadge).pos(18,12)).display();
                    Bagl::LABELLINE(LabelLine::new().text("Confirm").pos(0, 20)).paint();
                    LEFT_ARROW.paint();
                    RIGHT_ARROW.paint();
                }
                PromptingState::Cancel => {
                    trace!("Showing cancel");
                    Bagl::ICON(Icon::new(Icons::CrossBadge).pos(18,12)).display();
                    Bagl::LABELLINE(LabelLine::new().text("Cancel").pos(0, 20)).paint();
                    LEFT_ARROW.paint();
                }
            }
            // Handle buttons
            /* let buttons_evt = loop { match self.io.get_comm().unwrap().next_event::<u8>() {
                Event::Button(button) => { break button.clone(); }
                _ => { }
            } }; */

            trace!("Getting next button event");
            // let buttons_evt = self.io.next_button().await;
            trace!("Post button state: {:?}", self);
            loop {
            if let Some(buttons_evt) = nanos_ui::ui::get_event(&mut buttons) {
            // {
                // trace!("Received buttons {:?}", (state.clone(), buttons_evt));
                match (state.clone(), buttons_evt) {
                    (PromptingState::Prompts, ButtonEvent::LeftButtonRelease) => {
                        trace!("Backward");
                        if backward.prev != [0; 32] {
                            forward.add_prompt_chunk(&currentTitle, &currentBody).await?;
                            title_and_body = backward.pop().await?.unwrap();
                        }
                    }
                    (PromptingState::Prompts, ButtonEvent::RightButtonRelease) => {
                        trace!("Forward");
                        if forward.prev != [0; 32] {
                            trace!("Adding current prompt page to reverse queue");
                            backward.add_prompt_chunk(&currentTitle, &currentBody).await?;
                            trace!("Getting next prompt page");
                            title_and_body = forward.pop().await?.unwrap();
                        } else {
                            trace!("Prompts show, going to confirm");
                            state = PromptingState::Confirm;
                        }
                    }
                    (PromptingState::Confirm, ButtonEvent::LeftButtonRelease) => {
                        state = PromptingState::Prompts;
                    }
                    (PromptingState::Confirm, ButtonEvent::RightButtonRelease) => {
                        state = PromptingState::Cancel;
                    }
                    (PromptingState::Confirm, ButtonEvent::BothButtonsRelease) => {
                        return Ok(true);
                    }
                    (PromptingState::Cancel, ButtonEvent::BothButtonsRelease) => {
                        return Ok(false);
                    }
                    (PromptingState::Cancel, ButtonEvent::LeftButtonRelease) => {
                        state = PromptingState::Confirm;
                    }
                    _ => { }
                }
                trace!("Buttons handled");
                break;
            } else {
                // trace!("Some other event");
                }
            }
        }
    }

    async fn add_prompt_chunk(&mut self, title: &str, segment: &str) -> Result<(), PromptingError> {
        if title.len() > 17 { return Err(PromptingError); }
        if segment.len() > PROMPT_CHUNK_LENGTH { return Err(PromptingError); }
        // trace!("Adding prompt chunk: {}, {}", title, segment);
        let mut chunk = PromptBuffer::new();
        chunk.try_extend_from_slice(&self.prev);
        chunk.push(title.len() as u8);
        chunk.push(segment.len() as u8);
        // trace!("CHUNK: {:?}", chunk);
        chunk.try_extend_from_slice(title.as_bytes()).unwrap();
        // trace!("CHUNK: {:?}", chunk);
        chunk.try_extend_from_slice(segment.as_bytes()).unwrap();
        // trace!("CHUNK: {:?}", chunk);
        trace!("Buffer is: {:?}, segment: {}", chunk, segment);
        self.prev = self.io.put_chunk(&chunk).await;
        trace!("Chunk put.");
        Ok(())
    }

    pub async fn add_prompt(&mut self, title: &str, args: Arguments<'_>) -> Result<(), PromptingError> {
        let mut writer = ChunkedWrite::new();
        while !writer.terminated() {
            core::fmt::write(&mut writer, args);
            self.add_prompt_chunk(title, writer.get_buffer()).await?;
            writer.advance();
        }
        trace!("Prompt saved");
        Ok(())
    }
}

#[derive(Debug)]
struct ChunkedWrite { 
    n: usize,
    i: usize,
    terminated: bool,
    buffer: ArrayString<PROMPT_CHUNK_LENGTH>

}

impl ChunkedWrite {
    fn new() -> ChunkedWrite {
        ChunkedWrite {
            n: 0,
            i: 0,
            terminated: false,
            buffer: ArrayString::new()
        }
    }
    fn advance(&mut self) {
        self.terminated = self.i <= self.n+PROMPT_CHUNK_LENGTH;
        self.i=0;
        self.n=self.n+PROMPT_CHUNK_LENGTH;
    }
    fn terminated(&self) -> bool {
        self.terminated
    }
    fn get_buffer(&self) -> &str {
        &self.buffer
    }
}

impl Write for ChunkedWrite {
    fn write_str(&mut self, s: &str) -> Result<(), Error> {
        let n_in_string = self.n as isize - self.i as isize;
        let ns_in_string = n_in_string + PROMPT_CHUNK_LENGTH as isize;
        // let amount = core::cmp::min( s.len() - n_in_string,  PROMPT_CHUNK_LENGTH );
        // trace!("Indices: {:?}", (core::cmp::max(0, n_in_string) as usize, core::cmp::min(s.len() as isize, ns_in_string) as usize));
        let pushed_slice = &s[core::cmp::max(0, n_in_string) as usize ..core::cmp::min(s.len() as isize, ns_in_string) as usize];
        // trace!("Slice: {}", pushed_slice);
        if pushed_slice.len() > 0 { // n_in_string < s.len() && amount > 0 {
            self.buffer.push_str(pushed_slice);
        }
        self.i = self.i + pushed_slice.len();
        Ok(())
    }
}

