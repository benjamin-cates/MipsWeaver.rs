.text
lw     $t0, 4($gp) ;      # fetch N
mult   $t0, $t0, $t0 ;    # N*N
lw     $t1, 4($gp)   ;    # fetch N
ori    $t2, $zero, 3 ;    # 3
mul    $t1, $f2 ;    # 3*N
add    $t222, $t0, $t1 ;    # N*N + 3*N
