SECTIONS
{
    .rodata.lispm ALIGN(16) : {
        PROVIDE(lispm_builtins = .);
        KEEP (*(.lispm.rodata.builtins.core))
        KEEP (*(.lispm.rodata.builtins.ext))
        . += 32;
    } = 0
}
INSERT AFTER .rodata;