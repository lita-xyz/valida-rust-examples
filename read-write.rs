#![no_std]
#![feature(start)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {

    extern { fn read_stdin() -> u32;}
    extern { fn write_stdout(n: u32);}

    unsafe {
    let n = read_stdin();
    write_stdout(n);
    }
    return 0;
}

