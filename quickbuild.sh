if [ `hostname` = "justin-laptop" ]
then PDF_VIEWER=okular
else PDF_VIEWER=evince
fi

function clean_latex
{
    filename=$1
    (rm -f "$filename.log") &&
        (rm -f "$filename.out") &&
        (rm -f "$filename.aux")
}

function uctex # (upgraded astronomy)
{
    file=$1
    extension=${file##*.}
    filename=${file%.*}
    pdflatex $file
    (clean_latex $filename)
}

uctex thesis.tex
$PDF_VIEWER thesis.pdf &> /dev/null &
while inotifywait -e close_write thesis.tex justin.bib chapters/*.tex; do uctex thesis.tex; done

