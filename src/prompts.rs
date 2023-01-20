use arrayvec::ArrayString;
use crate::*;
use core::fmt::{Arguments, Write, Error};

use nanos_ui::{bagls::*};
use nanos_sdk::buttons::*;

#[derive(Clone, Debug)]
pub struct PromptQueue {
    io: HostIO,
    prev: SHA256Sum,
}

#[cfg(target_os = "nanos")]
const PROMPT_CHUNK_LENGTH : usize = 16;

#[cfg(not(target_os = "nanos"))]
const PROMPT_CHUNK_LENGTH : usize = 64;

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

    pub async fn append(&mut self, other: &PromptQueue) -> Result<(), PromptingError> {
        let mut from = other.reverse().await?;
        while let Some((title, body)) = from.pop().await? {
            self.add_prompt_chunk(&title, &body).await;
        }
        Ok(())
    }

    pub async fn show(&mut self) -> Result<bool, PromptingError> {
        if self.prev == [0; 32] { return Err(PromptingError); } // No showing empty PromptQueues.
        
        let mut forward = self.reverse().await?;
        let mut backward = Self::new(self.io);
        let mut title_and_body = forward.pop().await?.expect("already checked for this?");

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
            let (current_title, current_body) = title_and_body;
            match state {
                PromptingState::Prompts => {
                    Bagl::LABELLINE(LabelLine::new().pos(0, 10).text(current_title.as_str())).display();
                    #[cfg(target_os = "nanos")]
                        {
                            Bagl::LABELLINE(LabelLine::new().pos(0, 25).text(current_body.as_str())).paint();
                        }
                    #[cfg(not(target_os = "nanos"))]
                        {
                            current_body.as_str().get(0 .. 16).map(
                            |body| Bagl::LABELLINE(LabelLine::new().pos(0, 25).text(body)).paint()
                            );
                            current_body.as_str().get(16 .. 32).map(
                            |body| Bagl::LABELLINE(LabelLine::new().pos(0, 37).text(body)).paint()
                            );
                            current_body.as_str().get(32 .. 48).map(
                            |body| Bagl::LABELLINE(LabelLine::new().pos(0, 49).text(body)).paint()
                            );
                            current_body.as_str().get(48 .. 64).map(
                            |body| Bagl::LABELLINE(LabelLine::new().pos(0, 61).text(body)).paint()
                            );
                        }
                    if backward.prev != [0; 32] {
                        LEFT_ARROW.paint();
                    }
                    RIGHT_ARROW.paint();
                }
                PromptingState::Confirm => {
                    Bagl::ICON(Icon::new(Icons::CheckBadge).pos(18,12)).display();
                    Bagl::LABELLINE(LabelLine::new().text("Confirm").pos(0, 20)).paint();
                    LEFT_ARROW.paint();
                    RIGHT_ARROW.paint();
                }
                PromptingState::Cancel => {
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

            // let buttons_evt = self.io.next_button().await;
            loop {
            if let Some(buttons_evt) = nanos_ui::ui::get_event(&mut buttons) {
            // {
                match (state.clone(), buttons_evt) {
                    (PromptingState::Prompts, ButtonEvent::LeftButtonRelease) => {
                        if backward.prev != [0; 32] {
                            forward.add_prompt_chunk(&current_title, &current_body).await?;
                            title_and_body = backward.pop().await?.unwrap();
                        }
                    }
                    (PromptingState::Prompts, ButtonEvent::RightButtonRelease) => {
                        if forward.prev != [0; 32] {
                            backward.add_prompt_chunk(&current_title, &current_body).await?;
                            title_and_body = forward.pop().await?.unwrap();
                        } else {
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
                break;
            } else {
                }
            }
        }
    }

    async fn add_prompt_chunk(&mut self, title: &str, segment: &str) -> Result<(), PromptingError> {
        if title.len() > 17 { return Err(PromptingError); }
        if segment.len() > PROMPT_CHUNK_LENGTH { return Err(PromptingError); }
        let mut chunk = PromptBuffer::new();
        chunk.try_extend_from_slice(&self.prev);
        chunk.push(title.len() as u8);
        chunk.push(segment.len() as u8);
        chunk.try_extend_from_slice(title.as_bytes()); // .unwrap();
        chunk.try_extend_from_slice(segment.as_bytes()); // .unwrap();
        self.prev = self.io.put_chunk(&chunk).await;
        Ok(())
    }

    pub async fn add_prompt(&mut self, title: &str, args: Arguments<'_>) -> Result<(), PromptingError> {
        let mut writer = ChunkedWrite::new();
        while !writer.terminated() {
            core::fmt::write(&mut writer, args);
            self.add_prompt_chunk(title, writer.get_buffer()).await?;
            writer.advance();
        }
        Ok(())
    }
}

#[derive(Debug)]
struct ChunkedWrite { 
    skip: usize,
    skip_this_pass: usize,
    len: usize,
    terminated: bool,
    buffer: ArrayString<PROMPT_CHUNK_LENGTH>

}

impl ChunkedWrite {
    fn new() -> ChunkedWrite {
        ChunkedWrite {
            skip: 0,
            skip_this_pass: 0,
            len: 0,
            terminated: false,
            buffer: ArrayString::new()
        }
    }
    fn advance(&mut self) {
        self.terminated = self.skip + self.buffer.len() >= self.len;
        self.buffer.clear();
        self.skip += PROMPT_CHUNK_LENGTH;
        self.skip_this_pass = self.skip;
        self.len = 0;
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
        self.len += s.len();
        
        if self.skip_this_pass > s.len() {
            self.skip_this_pass -= s.len();
            return Ok(());
        }
        if self.buffer.is_full() {
            return Ok(());
        }
        let pushed_slice = &s[self.skip_this_pass .. core::cmp::min(s.len(), PROMPT_CHUNK_LENGTH + self.skip_this_pass - self.buffer.len())];
        self.skip_this_pass = core::cmp::max(0, self.skip_this_pass as isize - s.len() as isize) as usize;

        self.buffer.try_push_str(pushed_slice);
        Ok(())
    }
}

