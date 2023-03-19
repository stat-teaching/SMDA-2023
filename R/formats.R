#' @export
pdfslides <- function(...) {
    template <- system.file("rmarkdown/templates/exam/resources/examtemplate.tex",
                            package="monash")
    bookdown::pdf_document2(...,
                            template = template
    )
}
