SECTIONS
{
    .rodata.lispm ALIGN(16) : {
        PROVIDE(lispm_builtins_start = .);
        KEEP (*(.lispm.rodata.builtins.core))
        KEEP (*(.lispm.rodata.builtins.ext))
        PROVIDE(lispm_builtins_end = .);
    } = 0
}
INSERT AFTER .rodata;