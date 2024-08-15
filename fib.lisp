(LET
    (
        (MUL
            (LAMBDA
                (X Y)
                (CAPI 'MUL X Y)
            )
        )
        (SUB
            (LAMBDA
                (X Y)
                (CAPI 'SUB X Y)
            )
        )
        (FIB_R
            (LAMBDA
                (ACC I REC)
                (COND
                    ((EQ I '0) ACC)
                    ('T (REC (MUL A I) (SUB I '1) REC))
                )
            )
        )
    )
    (FIB_R 6 6 FIB_R)
)