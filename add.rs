#![no_std]
#![feature(start)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let x = 10;
    let y = 20;
    let z = x + y;
    if z == 30 {
        loop {}
    }
    else {
        return 0;
    }
}


