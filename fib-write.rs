#![no_std]
#![feature(start)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {

    #[link(name = "stdio")]
    extern { fn read_stdin() -> u32;}
    extern { fn write_stdout(n: u32);}

    unsafe {
    let n = read_stdin();
    let mut a: u32 = 0;
    let mut b: u32 = 1;
    let mut sum: u32;
    for _ in 1..n {
        sum = a + b;
        a = b;
        b = sum;
        }
    write_stdout(b);
    }
    return 0;
}

