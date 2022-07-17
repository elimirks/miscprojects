Code for [Ben Eater's 6502 computer kit](https://eater.net/6502)

## Memory map
- 8000-ffff: EEPROM
- fffc-fffd: must be set to 8000, to tell the processer to jump to that starting position
- fffe-ffff: Unused
- 6000-600f: IO chip. See section 2 of the data sheet for what each address does.
- 0000-00ff: "Zero page". Faster access memory than the rest of the address space
- 0100-01ff: Stack
- 0200-3fff: General purpose RAM

## Data sheets

- [65c02](https://eater.net/datasheets/w65c02s.pdf)
- [65c22](https://eater.net/datasheets/w65c22.pdf)

## 6502 Assembly

See `blink.s` for an minimal example program.
