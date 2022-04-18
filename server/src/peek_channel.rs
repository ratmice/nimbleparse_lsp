pub struct PeekableReceiver<T> {
    peeked: Option<T>,
    rx: tokio::sync::mpsc::UnboundedReceiver<T>,
}

impl<T> PeekableReceiver<T> {
    pub fn new(rx: tokio::sync::mpsc::UnboundedReceiver<T>) -> PeekableReceiver<T> {
        PeekableReceiver { peeked: None, rx }
    }

    pub fn peek(&mut self) -> Option<&T> {
        if self.peeked.is_none() {
            if let Ok(x) = self.rx.try_recv() {
                self.peeked = Some(x);
                self.peeked.as_ref()
            } else {
                None
            }
        } else {
            self.peeked.as_ref()
        }
    }

    pub fn try_recv(&mut self) -> Result<T, tokio::sync::mpsc::error::TryRecvError> {
        if let Some(x) = self.peeked.take() {
            self.peeked = None;
            Ok(x)
        } else {
            self.rx.try_recv()
        }
    }

    pub fn blocking_recv(&mut self) -> Option<T> {
        if let Some(x) = self.peeked.take() {
            self.peeked = None;
            Some(x)
        } else {
            self.rx.blocking_recv()
        }
    }

    // This is pretty terrible
    pub fn conditional_blocking_recv(&mut self, block: bool) -> Option<T> {
        if block {
            self.blocking_recv()
        } else {
            self.try_recv().ok()
        }
    }
}
