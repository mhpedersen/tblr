

tbl <- function(d, row.names=T, col.names=T, corner="", ...) {
    if (!is.data.frame(d)) stop("X must be data.frame")  # coerce to data.frame

    structure( list(data=d,
                    formats=array(rep(list(NULL),prod(dim(d))),dim=dim(d)),
                    master_format=cell_format(...),
                    cols=list(),
                    hlines=list(),
                    vlines=list(),
                    row.names=row.names,
                    col.names=col.names,
                    corner=corner,
                    frame=NULL,
                    row0.linestyle=NULL,
                    col0.linestyle=NULL
    ),
    class = "tbl")
}

# borders  (not clear what style is)
cell_format <- function(color=NULL, background=NULL, font=NULL, style=NULL, align=NULL, format=NULL, width=NULL)
    structure(list(color=color, background=background, font=font, style=style, align=align, format=format, width=width),
              class="cell_format")

column <- function(colname, ...)
    structure(list(
        colname=colname,
        format=cell_format(...)
    ),
    class="column")

#columns

cell <- function(r, c, ...) cells(list(c(r,c)),...)

cells <- function(coords, ...)
    structure(list(
        coords=coords,
        format=cell_format(...)
    ),
    class="cells")

hlines <- function(rows, style="1px solid black") structure(list(rows=rows, style=style), class="hlines")

hline <- function(row, ...) hlines(c(row), ...)

vlines <- function(cols, style="1px solid black") structure(list(cols=cols, style=style), class="vlines")

vline <- function(col, ...) vlines(c(col), ...)

# allow different style for each side
frame <- function(top=TRUE, bottom=TRUE, left=TRUE, right=TRUE, style="1px solid")
    structure(list(top=top, bottom=bottom, left=left, right=right, style=style
    ),
    class="frame")

grid <- function(row.begin=1, col.begin=1, row.step=1, col.step=1, style="1px dashed lightgrey")
    structure(list(row.begin=row.begin, col.begin=col.begin, row.step=row.step, col.step=col.step, style=style
    ),
    class="grid")

print.tbl <- function(t,...){   # generate html, latex, or console output
    width_master <- ifelse(is.null(t$master_format$width), "", paste0(' width=',t$master_format$width) )
    align_master <- ifelse(is.null(t$master_format$align), "", paste0(' align="',t$master_format$align,'"') )

    fstyle <- NULL
    if(!is.null(t$frame)){
        if(t$frame$left) fstyle <- c(fstyle, paste0("border-left:",t$frame$style))
        if(t$frame$right) fstyle <- c(fstyle, paste0("border-right:",t$frame$style))
        if(t$frame$top) fstyle <- c(fstyle, paste0("border-top:",t$frame$style))
        if(t$frame$bottom) fstyle <- c(fstyle, paste0("border-bottom:",t$frame$style))
        if(!is.null(fstyle))
            fstyle <- paste0('style="',do.call(paste, as.list(c(fstyle, sep=";"))), '"')
    }
    cat('<TABLE rules="groups" ', fstyle, '">\n')

    # Generate table headers
    hline <- ifelse(is.null(t$col0.linestyle), "", paste0(' style="',t$col0.linestyle,'"'))
    vline <- ifelse(is.null(t$row0.linestyle), "", paste0(' style="',t$row0.linestyle,'"'))
    cat('<TR', hline, '>\n')
    if(t$row.names) cat('<TH', vline, '>',t$corner,'</TH>\n')   # this could also have alignment and width
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
            cat('<TH',vline,width,align,'>', s, '</TH>', sep='')
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
            cat('<TH', vline, '>',rownames(t$data)[i],'</TH>', sep='')
        }
        for(j in 1:ncol(t$data)){
            #vline <- t$vlines[j][[1]]
            #vline <- ifelse(is.null(vline), "", paste0(' style="',vline,'"'))
            style <- t$vlines[j][[1]]

            align <- align_master
            fmt <- t$formats[[i,j]]
            if(!is.null(fmt)){
                if(!is.null(fmt$align))
                    align <- paste0(' align="',fmt$align,'"')
                if(!is.null(fmt$background))
                    style <- c(style, paste0("background-color:",fmt$background))
                if(!is.null(fmt$color))
                    style <- c(style, paste0("color:",fmt$color))            }
            if(!is.null(style))
                style <- paste0('style="',do.call(paste, as.list(c(style, sep=";"))), '"')

            cat('<TD',align,style,'>', t$data[i,j], '</TD>', sep='')
        }
        cat('</TR>\n')
    }

    cat("</TABLE>")
}

#adding other tbl classes should append them into a format container
Ops.tbl <- function(t, x){
    if(.Generic!="+")
        stop(paste0(.Generic," not implemented for tbl"))

    if(class(x)=="column")
        t$cols[[x$colname]] <- x$format
    else if(class(x)=="cells")
        for(idx in x$coords)
            t$formats[[idx[1], idx[2]]] <- x$format
    else if(class(x)=="hlines"){
        style <- paste0("border-bottom:",x$style)
        for(idx in x$rows)
            if(idx==0)
                t$col0.linestyle <- style
        else
            t$hlines[[idx]] <- style
    }
    else if(class(x)=="vlines"){
        style <- paste0("border-right:",x$style)
        for(idx in x$cols)
            if(idx==0)
                t$row0.linestyle <- style
        else
            t$vlines[[idx]] <- style
    }
    else if(class(x)=="frame")
        t$frame <- x
    else if(class(x)=="grid"){
        for(i in seq(x$row.begin, dim(t$data)[1], x$row.step))
            t$hlines[[i]] <- paste0("border-bottom:",x$style)
        for(i in seq(x$col.begin, dim(t$data)[2], x$col.step))
            t$vlines[[i]] <- paste0("border-right:",x$style)
    } else
        stop("Can not add to tbl: ", class(x))

    t
}
