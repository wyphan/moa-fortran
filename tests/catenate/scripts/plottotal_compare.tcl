# plottotal.tcl --
#     Plot the total time spent on the calculations
#
package require Plotchart
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl


set type "native"
set title "Total time - -march=native"

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
        if { [string first "CPU time" $line] >= 0 } {
            set time [lindex $line end]
            set total [expr {$total + $time}]
        }
    }

    close $infile

    return $total
}

set p [::Plotchart::createLogXLogYPlot .c {1.0e4 1.0e10 10.0} {10.0 1.0e4 10.0}]
#$p xconfig -format %.4e

$p title $title
$p dataconfig array   -colour blue    -symbol plus   -type both
$p dataconfig view1   -colour magenta -symbol cross  -type both
$p dataconfig view2   -colour lime    -symbol circle -type both

$p legendconfig -spacing 17
$p legend array   "Plain arrays"
$p legend view1   "View (10 pieces)"
$p legend view2   "View (100 pieces)"

foreach file {array1 array2 array3 array4 array5
              view1  view2  view3  view4  view5
              view1b view2b view3b view4b view5b} \
        size {1.0e5  1.0e6  1.0e7  1.0e8  1.0e9
              1.0e5  1.0e6  1.0e7  1.0e8  1.0e9
              1.0e5  1.0e6  1.0e7  1.0e8  1.0e9 } {
    set total [getTotal $file.out]

    set label "array"
    if { [string match "view*" $file] } {
        set label "view1"
    }
    if { [string match "view*b" $file] } {
        set label "view2"
    }

    $p plot $label $size $total
}


after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {16c 12c}]
    $pdf1 canvas .c
    $pdf1 write -file total_$type.pdf
}

