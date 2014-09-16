#' Apply formatting to data.frame content
#' @export
format_data <- function(d, colFormats=NULL, typeFormats=NULL, useOptions=TRUE, na.rm=getOption("tblr.na.rm",TRUE)){
    assign("fd", d)

    if(useOptions){
        default_typeFormats <- getOption("tblr.typeFormats")
        default_colFormats <- getOption("tblr.colFormats")
    } else {
        default_typeFormats <- NULL
        default_colFormats  <- NULL
    }

    for(i in 1:ncol(d)){
        if(names(d)[[i]] %in% names(colFormats))
            fmt <- colFormats[[names(d)[[i]]]]
        else if(names(d)[[i]] %in% names(default_colFormats))
            fmt <- default_colFormats[[names(d)[[i]]]]
        else if(class(d[[i]]) %in% names(typeFormats))
            fmt <- typeFormats[[class(d[[i]])]]
        else if(class(d[[i]]) %in% names(default_typeFormats))
            fmt <- default_typeFormats[[class(d[[i]])]]
        else
            fmt <- NULL

        if(class(fmt) %in% c("NULL", "list")){
            fmt$x <- d[[i]]
            fd[[i]] <- do.call("format", fmt)
        } else if(class(fmt)=="function")
            fd[[i]] <- fmt(d[[i]])
        else if(class(fmt)=="character")
            fd[[i]] <- sprintf(fmt, d[[i]])

        rm <- NULL
        if(class(na.rm)=="logical")
            rm <- na.rm
        else if(class(na.rm)=="list" && names(d)[[i]] %in% names(na.rm))
            rm <- na.rm[[names(d)[[i]]]]
        if(class(rm)=="logical"){
            if(rm) rm <- ""
            else   rm <- NULL
        }
        if(!is.null(rm))
            fd[[i]][is.na(d[[i]])] <- rm
    }

    fd
}

#' Convert certain characters to HTML encoding
#'
#' Convert certain characters to HTML encoding, eg. '<' becomes '&amp;'
#' Special symbols can be specified as LaTeX (TODO)
#'
#' @references
#' \url{http://www.escapecodes.info}
#'
#' @export
HTMLencode <- function(s){
    conversion <- list(
        c("&", "&amp;"),
        c("<", "&lt;"),
        c(">", "&gt;"),
        c("'", "&rsquo;"),
        c('"', "&quot;"),
        c("£", "&pound;"),
        c("€", "&euro;")
        )
    for(c in conversion)
        s <- gsub(c[[1]], c[[2]], s)
    s
}

# regex? to match cols

#options(tblr.typeFormats=list(Date    = list(format="%d %b %Y"),
#                             numeric = list(digits=0, scientific=FALSE, big.mark=",")))

#format_data(t, colFormats=list(x=function(x) sprintf("%0.2f%%",x*100),
#                               OPB="%0.7f"
#                               ), na.rm=list(x="-",UPB=T,OPB=T) )



#aov, lm, anova, glm etc  (from xtable, potentially more; rpart, surv...), time series

#' @export
tblr <- function(x, ...) UseMethod("tblr")
#' @export
tblr.default <- function(d, ...) tblr(as.data.frame(d), ...)
#' @export
tblr.table <- function(t, ...)
    switch(length(dim(d)),
        tblr(data.frame(as.list(t), check.names=FALSE), ...), # 1D
        tblr(as.data.frame.matrix(t), ...), # 2D
        tblr(as.data.frame(t), ...) # default; >2D
    )


#' Create a table object from a data.frame
#' @export
tblr.data.frame <- function(d, colFormats=NULL, typeFormats=NULL, useOptions=TRUE, na.rm=getOption("tbl.na.rm",TRUE),
                row.names=NULL, col.names=T, corner="", ...) {
    d <- format_data(d, colFormats, typeFormats, useOptions, na.rm)
    if(is.null(row.names))
        row.names = !is.null(rownames(d)) && !identical(rownames(d), as.character(seq_len(nrow(d))))
    structure( list(data=d,
                    formats=array(rep(list(NULL),prod(dim(d))),dim=dim(d)),
                    master_format=cell_format(...),
                    cols=list(),
                    rows=list(),
                    hlines=list(),
                    vlines=list(),
                    row.names=row.names,
                    col.names=col.names,
                    corner=corner,
                    frame=NULL,
                    row0.linestyle=NULL,
                    col0.linestyle=NULL,
                    row0.format=NULL,
                    col0.format=NULL,
                    caption=NULL
    ),
    class = "tblr")
}

# align, width... operator

#' cell_format
#' @export
cell_format <- function(color=NULL, background=NULL, font=NULL, style=NULL, align=NULL, format=NULL, width=NULL)
    structure(list(color=color, background=background, font=font, style=style, align=align, format=format, width=width),
              class="cell_format")

#' caption
#' @export
caption <- function(text, top=FALSE,  ...)
    structure(list(
            text=text,
            top=top,
            format=cell_format(...)
        ),
        class="caption")


#' columns
#' @export
columns <- function(colnames, ...)
    structure(list(
        colnames=colnames,
        format=cell_format(...)
    ),
    class="columns")

#' column
#' @export
column <- function(colname, ...) columns(c(colname), ...)


#' rows
#' @export
rows <- function(rows, ...)
    structure(list(
        rows=rows,
        format=cell_format(...)
    ),
    class="rows")

#' row
#' @export
row <- function(r, ...) rows(c(r), ...)




#' cell
#' @export
cell <- function(r, c, ...) cells(list(c(r,c)),...)

#' cells
#' @export
cells <- function(coords, ...)
    structure(list(
        coords=coords,
        format=cell_format(...)
    ),
    class="cells")

#' hlines
#' @export
hlines <- function(rows, style="1px solid black") structure(list(rows=rows, style=style), class="hlines")

#' hline
#' @export
hline <- function(row, ...) hlines(c(row), ...)

#' vlines
#' @export
vlines <- function(cols, style="1px solid black") structure(list(cols=cols, style=style), class="vlines")

#' vline
#' @export
vline <- function(col, ...) vlines(c(col), ...)

#' frame
#' @export
frame <- function(top=TRUE, bottom=TRUE, left=TRUE, right=TRUE, style="1px solid")
    structure(list(top=top, bottom=bottom, left=left, right=right, style=style
    ),
    class="frame")

#' grid
#' @export
grid <- function(row.begin=1, col.begin=1, row.step=1, col.step=1, style="1px dashed lightgrey")
    structure(list(row.begin=row.begin, col.begin=col.begin, row.step=row.step, col.step=col.step, style=style
    ),
    class="grid")

`%||%` <- function(a, b) if(is.null(a)) b else a

#' print.tbl
#' @export
print.tblr <- function(t,...){   # generate html, latex, or console output
    w <- t$master_format$width %||% getOption("tblr.width")
    width_master <- ifelse(is.null(w), "", paste0(' width=',w) )
    a <- t$master_format$align %||% getOption("tblr.align")
    align_master <- ifelse(is.null(a), "", paste0(' align="',a,'"') )

    fstyle <- NULL
    if(!is.null(t$frame)){
        if(t$frame$left) fstyle <- c(fstyle, paste0("border-left:",t$frame$style))
        if(t$frame$right) fstyle <- c(fstyle, paste0("border-right:",t$frame$style))
        if(t$frame$top) fstyle <- c(fstyle, paste0("border-top:",t$frame$style))
        if(t$frame$bottom) fstyle <- c(fstyle, paste0("border-bottom:",t$frame$style))
        if(!is.null(fstyle))
            fstyle <- paste0('style="',do.call(paste, as.list(c(fstyle, sep=";"))), '"')
    }
    cat('<TABLE rules="groups" ', fstyle, '>\n')

    if(!is.null(t$caption)){
        style <- paste0("caption-side: ", if(t$caption$top) "top" else "bottom")
        if(!is.null(t$caption$format$align)) style <- c(style, paste0("text-align:",t$caption$format$align))
        if(!is.null(t$caption$format$color)) style <- c(style, paste0("color:",t$caption$format$color))
        if(!is.null(t$caption$format$background)) style <- c(style, paste0("background:",t$caption$format$background))
        style <- paste0('style="',do.call(paste, as.list(c(style, sep=";"))), '"')
        cat('<CAPTION ', style, '>', t$caption$text, '</CAPTION>')
    }

    # Generate table headers
    hline <- ifelse(is.null(t$col0.linestyle), "", paste0(' style="',t$col0.linestyle,'"'))
    vline <- ifelse(is.null(t$row0.linestyle), "", paste0(' style="',t$row0.linestyle,'"'))
    cat('<TR', hline, '>\n')
    if(t$row.names) cat('<TH', vline, '>',HTMLencode(t$corner),'</TH>\n')   # this could also have alignment and width
    c <- 1
    for(s in colnames(t$data)){
        fmt <- t$cols[[s]]
        align <- align_master
        width <- width_master

        vline <- t$vlines[c][[1]]
        vline <- ifelse(is.null(vline), "", paste0(' style="',vline,'"'))

        if(!is.null(fmt)){
            if(!is.null(fmt$width))
                width <- paste0(' width=',fmt$width)
            if(!is.null(fmt$align))
                align <- paste0(' align="',fmt$align,'"')
        }
        if(t$col.names)
            cat('<TH',vline,width,align,'>', HTMLencode(s), '</TH>', sep='')
        else
            cat('<TH',vline,width,align,'></TH>', sep='')

        c <- c+1
    }
    cat('\n</TR>\n')

    # Main table
    for(i in 1:nrow(t$data)){
        # formatting firstly from cells, then cols, then rows, then global

        hline <- t$hlines[i][[1]]
        hline <- ifelse(is.null(hline), "", paste0(' style="',hline,'"'))
        cat('<TR',hline,'>', sep='')

        if(t$row.names){
            vline <- ifelse(is.null(t$row0.linestyle), "", paste0(' style="',t$row0.linestyle,'"'))
            cat('<TH', vline, '>', HTMLencode(rownames(t$data)[i]), '</TH>', sep='')
        }
        for(j in 1:ncol(t$data)){
            style <- t$vlines[j][[1]]

            align <- align_master
            fmt <- t$formats[[i,j]]
            if(!is.null(fmt)){
                if(!is.null(fmt$align))
                    align <- paste0(' align="',fmt$align,'"')
                if(!is.null(fmt$background))
                    style <- c(style, paste0("background-color:",fmt$background))
                if(!is.null(fmt$color))
                    style <- c(style, paste0("color:",fmt$color))
            }
            if(!is.null(style))
                style <- paste0('style="',do.call(paste, as.list(c(style, sep=";"))), '"')

            cat('<TD',align,style,'>', HTMLencode(format(t$data[i,j])), '</TD>', sep='')
        }
        cat('</TR>\n')
    }

    cat("</TABLE>")
}

#' @export
Ops.tblr <- function(t, x){
    if(.Generic!="+")
        stop(paste0(.Generic," not implemented for tblr"))

    if(class(x)=="columns")
        for(c in x$colnames)
            t$cols[[c]] <- x$format
    else if(class(x)=="rows")
        for(r in x$rows)
            t$rows[[r]] <- x$format
    else if(class(x)=="cells")
        for(idx in x$coords)
            t$formats[[idx[1], idx[2]]] <- x$format
    else if(class(x)=="hlines"){
        style <- paste0("border-bottom:",x$style)
        t$hlines[c(x$rows[x$rows>0], x$rows[x$rows<0]+nrow(t$data))]  <- style
        if(0 %in% x$rows)
            t$col0.linestyle <- style
    }
    else if(class(x)=="vlines"){
        style <- paste0("border-right:",x$style)
        t$vlines[c(x$cols[x$cols>0], x$cols[x$cols<0]+ncol(t$data))]  <- style
        if(0 %in% x$cols)
            t$row0.linestyle <- style
    }
    else if(class(x)=="frame")
        t$frame <- x
    else if(class(x)=="caption")
        t$caption <- x
    else if(class(x)=="grid"){
        for(i in seq(x$row.begin, dim(t$data)[1]-1, x$row.step))
            t$hlines[[i]] <- paste0("border-bottom:",x$style)
        for(i in seq(x$col.begin, dim(t$data)[2]-1, x$col.step))
            t$vlines[[i]] <- paste0("border-right:",x$style)
    } else
        stop("Can not add to tbl: ", class(x))

    t
}
