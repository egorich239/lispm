SECTIONS
{
    .rodata.lispm ALIGN(16) : {
        PROVIDE(lispm_builtins = .);
        KEEP (lispm.o(.lispm.rodata.builtins.core))
        KEEP (*(.lispm.rodata.builtins.ext))
        . += 32;
    } = 0
}
INSERT AFTER .rodata;