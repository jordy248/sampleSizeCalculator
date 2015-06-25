new.power.prop.test <- function (n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05, power = NULL, 
                                 alternative = c("two.sided", "one.sided"), strict = FALSE, 
                                 tol = .Machine$double.eps^0.25) 
{
        if (sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 
            1) 
                stop("exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
        if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
                                                                 sig.level | sig.level > 1)) 
                stop("'sig.level' must be numeric in [0, 1]")
        alternative <- match.arg(alternative)
        tside <- switch(alternative, one.sided = 1, two.sided = 2)
        p.body <- if (strict && tside == 2) 
                quote({
                        qu <- qnorm(sig.level/tside, lower.tail = FALSE)
                        d <- abs(p1 - p2)
                        q1 <- 1 - p1
                        q2 <- 1 - p2
                        pbar <- (p1 + p2)/2
                        qbar <- 1 - pbar
                        v1 <- p1 * q1
                        v2 <- p2 * q2
                        vbar <- pbar * qbar
                        pnorm((sqrt(n) * d - qu * sqrt(2 * vbar))/sqrt(v1 + 
                                                                               v2)) + pnorm((sqrt(n) * d + qu * sqrt(2 * vbar))/sqrt(v1 + 
                                                                                                                                             v2), lower.tail = FALSE)
                })
        else quote(pnorm((sqrt(n) * abs(p1 - p2) - (qnorm(sig.level/tside, 
                                                          lower.tail = FALSE) * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))/sqrt(p1 * 
                                                                                                                                   (1 - p1) + p2 * (1 - p2))))
        if (is.null(power)) 
                power <- eval(p.body)
        else if (is.null(n)) 
                n <- uniroot(function(n) eval(p.body) - power, c(1, 1e+07), 
                             tol = tol, extendInt = "upX")$root
        else if (is.null(p1)) 
                p1 <- uniroot(function(p1) eval(p.body) - power, c(0, 
                                                                   p2), tol = tol, extendInt = "yes")$root
        else if (is.null(p2)) 
                p2 <- uniroot(function(p2) eval(p.body) - power, c(p1, 
                                                                   1), tol = tol, extendInt = "yes")$root
        else if (is.null(sig.level)) 
                sig.level <- uniroot(function(sig.level) eval(p.body) - 
                                             power, c(1e-10, 1 - 1e-10), tol = tol, extendInt = "upX")$root
        else stop("internal error", domain = NA)
        ##  NOTE <- "n is number in *each* group"
        ##  METHOD <- "Two-sample comparison of proportions power calculation"
        ##  structure(list(n = n, p1 = p1, p2 = p2, sig.level = sig.level, 
        ##                 power = power, alternative = alternative, note = NOTE, 
        ##                 method = METHOD), class = "power.htest")
}

calcSampSize <- function( csv = NULL, tails = "two.sided") {
        
        source('newpowerproptest.R')
        
        ## Get file
        if( is.null( csv ) ){
                files <- as.character( list.files( directory ) )
                stop( "Invalid file name" )
                
        } else {
                
                ## Load CSV  
                csv <- read.csv( csv, header = TRUE )
                
                ## Get unique forms
                form_uniques <- unique( csv[,1] )
                n_form_uniques <- length( form_uniques )
                
                cvr_uniques <- unique( csv[,2] )
                n_cvr_uniques <- length( cvr_uniques )
                
                ## Error handling
                ##  if( ( n_form_uniques == n_cvr_uniques ) == FALSE ) {
                ##    stop( "Each unique form must have exactly one control CVR")
                ##  }
                
                ## Create summary data frame
                summ_frame <- data.frame(Form_Name = rep( "", n_form_uniques ),
                                         Control_CVR = rep( "",n_form_uniques ),
                                         Tails = rep( "", n_form_uniques ),
                                         Confidence_Interval = rep( "",n_form_uniques ),
                                         Statistical_Power = rep( "", n_form_uniques ),
                                         Relative_Min_Detectable_Effect = rep( "", n_form_uniques ),
                                         VOLUME_PER_VARIATION = rep( "", n_form_uniques ),
                                         
                                         stringsAsFactors=FALSE )                 
                
                ## Populate summary data frame
                for( i in 1:nrow( csv ) ){
                        
                        form_name <- as.character( csv[i,1] )
                        control_cvr <- as.numeric( csv[i,2] )
                        alpha <- as.numeric( csv[i,3] )
                        beta <- as.numeric( csv[i,4] )
                        relative_mde <- as.numeric( csv[i,5] )
                        volume_per_variant <- new.power.prop.test(p1=control_cvr, p2=control_cvr * (1 + relative_mde), power=beta, alternative= tails, sig.level=alpha)
                        
                        summ_frame[i,] <- list( 
                                as.numeric( form_name ), 
                                as.numeric( control_cvr ),
                                as.character( tails ),
                                as.numeric( 1 - alpha ),
                                as.numeric( beta ),
                                as.numeric( relative_mde ),
                                as.numeric( volume_per_variant ) 
                        )
                        
                }
                summ_frame
                
                write.csv( summ_frame,"SAMPLE_SIZE_CALCUALATIONS.csv" )
                message(c( "Open the 'SAMPLE_SIZE_CALCULATIONS.csv' file in your working directory: ",getwd() ) )
        }
}

send_sample_size_results <- function (directory = NULL, file_name = NULL, recipient = NULL, subject = NULL){
        library(sendmailR)
        
        ## set wd to directory containing file to be sent
        setwd(directory)
        
        ## send plain email
        
        from <- "jnelson@humanesociety.org"
        to <- recipient
        subject <- subject
        body <- "Here's your file!"                     
        mailControl=list(smtpServer="serverinfo")
        
        sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
        
        ## attachment
        ## needs full path if not in working directory
        attachmentPath <- directory
        attachmentName <- file_name
        
        attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
        bodyWithAttachment <- list(body,attachmentObject)
        
        sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)
        
}
