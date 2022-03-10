# plottotal.tcl --
#     Plot the total time spent on the calculations
#
package require Plotchart
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl


set type "array"
set title "Total time - plain arrays"

pack [canvas .c -width 800 -height 600 -bg white]

proc getTotal {filename} {
    if { [catch {
        set infile [open $filename]
    }] } {
        puts "Missing: $filename"
        return {}
    }

    set total 0.0
    while { [gets $infile line] >= 0 } {
        if { [string first "Wall clock" $line] >= 0 } {
            set time [lindex $line end]
            set total [expr {$total + $time}]
        }
    }

    close $infile

    return $total
}

set p [::Plotchart::createLogXYPlot .c {1.0e4 1.0e10 10.0} {0.0 150.0 25.0}]
#$p xconfig -format %.4e

$p title $title
$p dataconfig windows -colour blue    -symbol plus   -type both
$p dataconfig virtual -colour magenta -symbol cross  -type both
$p dataconfig linux   -colour lime    -symbol circle -type both

$p legend windows "Plain Windows"
$p legend virtual "Virtual Windows"
$p legend linux   "Linux"

foreach dir   {.       horizon linux} \
        label {windows virtual linux} {
    foreach file {array1 array2 array3 array4 array5} \
            size {1.0e5  1.0e6  1.0e7  1.0e8  1.0e9 } {
        set total [getTotal [file join $dir $file.out]]

        $p plot $label $size $total
    }
}


after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {16c 12c}]
    $pdf1 canvas .c
    $pdf1 write -file total_$type.pdf
}

