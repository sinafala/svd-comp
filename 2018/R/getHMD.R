library(readr)
library(stringr)
library(httr)


list.raw.lts <- function(hmd.dir,age,period) {
  
  lts <- list(
    female = read.raw.lt(hmd.dir,"female",age,period),
    male = read.raw.lt(hmd.dir,"male",age,period),
    both = read.raw.lt(hmd.dir,"both",age,period)
  )
  
  return(lts)
  
}


read.raw.lt <- function(hmd.dir,sex,age,period) {
  
  # <hmd.dir> must contain a string that specifies the location 
  # of the 'hmd_statistics' directory created when the 
  # HMD .zip file is unzipped. 
  
  switch <- sex.per.age.switch(sex,age,period,hmd.dir)
  data.dir <- paste(hmd.dir,switch$data.path,sep="")
    
  lt <- list()
  
  files <- Sys.glob(paste(eval(data.dir),"/*.txt",sep=""))
  files.split <- strsplit(files,"\\.")
  
  for (i in 1:length(files.split)) {
    country.name <- strsplit(basename(files[i]),"\\.")[[1]][1]
    lt[[country.name]] = parse.lt(files[i],age)
  }
  
  return(lt)
  
}


sex.per.age.switch <- function(sex,age,period,root.dir) {
  
  subtract <- 0
  for (i in 1:3) {
    if(str_sub(root.dir,1,1)=="." | str_sub(root.dir,1,1)=="/") {
      subtract <- subtract+1
    }
  }
  root.length <- str_length(root.dir)-subtract
  
  if (sex == "female") {
    
    if (age == 1) {
      if (period == 1) {
        path <- "/lt_female/fltper_1x1"
        value <- list(
          sap.code = 1,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_female/fltper_1x5"
        value <- list(
          sap.code = 2,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_female/fltper_1x10"
        value <- list(
          sap.code = 3,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else if (age == 5) {
      if (period == 1) {
        path <- "/lt_female/fltper_5x1"
        value <- list(
          sap.code = 4,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_female/fltper_5x5"
        value <- list(
          sap.code = 5,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_female/fltper_5x10"
        value <- list(
          sap.code = 6,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else {
      value <- list(
        sap.code = 0,
        data.path = ""
      )
    }
    
  } else if (sex == "male") {
    
    if (age == 1) {
      if (period == 1) {
        path <- "/lt_male/mltper_1x1"
        value <- list(
          sap.code = 7,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_male/mltper_1x5"
        value <- list(
          sap.code = 8,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_male/mltper_1x10"
        value <- list(
          sap.code = 9,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else if (age == 5) {
      if (period == 1) {
        path <- "/lt_male/mltper_5x1"
        value <- list(
          sap.code = 10,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_male/mltper_5x5"
        value <- list(
          sap.code = 11,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_male/mltper_5x10"
        value <- list(
          sap.code = 12,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else {
      value <- list(
        sap.code = 0,
        data.path = ""
      )
    }
    
  } else if (sex == "both") {
    
    if (age == 1) {
      if (period == 1) {
        path <- "/lt_both/bltper_1x1"
        value <- list(
          sap.code = 7,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_both/bltper_1x5"
        value <- list(
          sap.code = 8,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_both/bltper_1x10"
        value <- list(
          sap.code = 9,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else if (age == 5) {
      if (period == 1) {
        path <- "/lt_both/bltper_5x1"
        value <- list(
          sap.code = 10,
          data.path = path
        )
      } else if (period == 5) {
        path <- "/lt_both/bltper_5x5"
        value <- list(
          sap.code = 11,
          data.path = path
        )
      } else if (period == 10) {
        path <- "/lt_both/bltper_5x10"
        value <- list(
          sap.code = 12,
          data.path = path
        )
      } else {
        value <- list(
          sap.code = 0,
          data.path = ""
        )
      }
    } else {
      value <- list(
        sap.code = 0,
        data.path = ""
      )
    }
    
  } else {
    value <- list(
      sap.code = 0,
      data.path = ""
    )
  }
  
  return(value)
  
}


parse.lt <- function(file.name,age) {
  
  if (age == 1) {
    w <- c(9, 11, 11, 9, 6, 8, 8, 8, 9, NA)
  } else if (age == 5) {
    w <- c(9, 11, 11, 9, 6, 8, 8, 8, 9, NA)
  } else {
    w <- NA
  }
  
  return(
    read_fwf(
      file=file.name
      , na = c("", "NA")
      , skip = 3
      , col_types="ccnnnnnnnn"
      , fwf_widths(
        widths = w
        , col_names=c("period","age","mx","qx","ax","lx","dx","Lx","Tx","ex")
      )
    )
  )
  
}


list.lts <- function(hmd.dir,age,period,download.date) {
  
  list.raw.lts <- list.raw.lts(hmd.dir,age,period)
  
  if (age == 1) {
    ages <- 111
  } else if (age == 5) {
    ages <- 24
  } else {
    ages <- NA
  }
  
  lt.list <- list()
  lt.list[["creation.date"]] <- date()
  lt.list[["download.date"]] <- download.date
  lt.list[["female"]] <- list()
  lt.list[["male"]] <- list()
  lt.list[["both"]] <- list()
  lt.list[["age"]] <- age
  lt.list[["age.groups"]] <- ages
  lt.list[["period"]] <- period

  for (sx in c("female","male","both")) {
    for (i in 1:length(list.raw.lts[[sx]])) {
      lt.list[[sx]][[eval(names(list.raw.lts[[sx]][i]))]] <- list()
      for (j in 1:(dim(list.raw.lts[[sx]][[i]])[1]/ages)) {
        pop <- eval(names(list.raw.lts[[sx]][i]))
        per <- unlist(list.raw.lts[[sx]][[i]][(ages*j-(ages-1)),1])
        per <- paste("P",str_replace_all(per, "[-]", "to"),sep="")
        lt.list[[sx]][[pop]][[per]] = list.raw.lts[[sx]][[i]][((ages*j-(ages-1)):(ages*j)),]
      }  
    }
  }
  
  return(lt.list)
  
}


count.lts <- function (lt.list,sex) {
  
  lt.cumsum <- 0
  for (i in 1:length(lt.list[[sex]])) {
    lt.cumsum <- lt.cumsum + length(lt.list[[sex]][[i]])
  }
  
  return(lt.cumsum)
  
}


extract.lt.col <- function (lt.list,sex,col.name) {
  
  if (lt.list$age == 1) {
    ages <- 111
  } else if (lt.list$age == 5) {
    ages <- 24
  } else {
    ages <- NA
  }
  
  if (col.name == "period") {
    col <- 1  
  } else if (col.name == "age") {
    col <- 2
  } else if (col.name == "mx") {
    col <- 3
  } else if (col.name == "qx") {
    col <- 4
  } else if (col.name == "ax") {
    col <- 5
  } else if (col.name == "lx") {
    col <- 6
  } else if (col.name == "dx") {
    col <- 7
  } else if (col.name == "Lx") {
    col <- 8
  } else if (col.name == "Tx") {
    col <- 9
  } else if (col.name == "ex") {
    col <- 10
  } else {
    col <- NA
  }
  
  lts.count <- count.lts(lt.list,"female")
  
  if (col > 1) {
    lts.colmat <- matrix(data=rep(0,ages*lts.count),nrow=ages,ncol=lts.count)
  } else if (col == 1) {
    lts.colmat <- matrix(data=rep("",ages*lts.count),nrow=ages,ncol=lts.count)
  }
  
  col.names <- rep("",lts.count)
  col.index <- 1
  
  for (i in 1:length(lt.list[[sex]])) {
    for (j in 1:length(lt.list[[sex]][[i]])) {
      if (col > 1) {
        lts.colmat[,col.index] <- as.numeric(unlist(lt.list[[sex]][[i]][[j]][,col]))
      } else if (col == 1) {
        lts.colmat[,col.index] <- as.character(unlist(lt.list[[sex]][[i]][[j]][,col]))
      }
      pop <- names(lt.list[[sex]])[i]
      per <- str_sub(names(lt.list[[sex]][[i]])[j],2,str_length(names(lt.list[[sex]][[i]])[j]))
      col.names[col.index] <- paste(eval(sex),".",pop,".",per,sep="")
      col.index <- col.index + 1
    }
  }
  
  colnames(lts.colmat) <- col.names
  rownames(lts.colmat) <- unlist(lt.list$female[[1]][[1]][,2])
  
  return(lts.colmat)
  
}


download.hmd <- function (output.file,unzip.dir,hmd.user,hmd.pass) {
  
  url <- "http://www.mortality.org/hmd/zip/all_hmd/hmd_statistics.zip"
  print("Downloading HMD ...")
  hmd.zip <- try(GET(url,authenticate(hmd.user,hmd.pass),write_disk(output.file,overwrite=TRUE)
                     ,progress(),show.error.messages=FALSE))
  if(class(hmd.zip)=="try-error") {
    return(hmd.zip)
    stop(attributes(hmd.zip)$condition)
  } else if (http_status(hmd.zip)$category=="Client error") {
    if(http_status(hmd.zip)$message=="Client error: (401) Unauthorized") {
      return(hmd.zip)
      stop("Check user name and password and try again.")    
    } else {
      return(hmd.zip)
      stop(http_status(hmd.zip)$message)
    }
  } 
  unzip(zipfile=output.file,exdir=unzip.dir)
  return(hmd.zip)
  
}


