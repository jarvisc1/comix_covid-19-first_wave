

## Loop questions are questions such as what is the age for X? 
## Where X may be each contact or household member or something like that.
## This function will seperate the loop questions into different
## datasets called tables. If the loop question is called Q23 then it will
## produce table_q23. 

reshape_q <- function(df, export_var_names = FALSE){
  
  # Check if empty_tables exist in global environment
  if(exists("empty_tables", envir = .GlobalEnv)){
    empty_tables <- get("empty_tables", envir = .GlobalEnv)
  } else {
    empty_tables <- NULL
  }
  
  ## Rename to lower case
  colnames(df) <- tolower(colnames(df))
  
  ## Reshape to long format
  df <- melt(
    df, 
    id.vars=c("respondent_id","wave", "qcountry"), 
    variable.factor=FALSE, 
    value.factor=FALSE
  )
  
  ## Sort the data by wave and then Id
  setorder(df, qcountry, wave, respondent_id)
  
  ## Trim white space in values
  df[, value := trimws(value)]
  
  ## Replace missing value with NA
  df[value == "", value := NA]
  
  ## Create a data table of the "loop" variable questions
  ## Sapply creates a list of loop variables names by splitting where "_" occurs
  questions_lists <- sapply(
    df[respondent_id==1 & grepl("loop", variable), variable], 
    strsplit, split="_"
  )
  
  ## Lapply creates a data.table by looping over the sapply and puting the 
  ## Different parts of the varibale names in different columns of the new
  ## data table
  questions_loop <- rbindlist(
    lapply(questions_lists,
           function(x){ as.data.table(t(c(paste0(x,collapse="_"),x))) }
    ),
    fill=TRUE
  )
  
  ## Rename the loop variables combine into one column then remove NAs
  questions_loop[, newname := paste0(V5,"_",V6,"_",V7,"_",V8)]
  questions_loop[, "newname"] <- sapply(
    strsplit(questions_loop[, newname], "_"),
    function(x){
      paste0(x[which(x != "NA")], collapse="_")
    }
  )
  
  ## Create a tablename variable for each table
  questions_loop[, "tablename" := paste0("table_",V2)]
  
  ## Create a seperate table for for loop question
  for(q in unique(questions_loop[, V2])){
    
    ## Pick data for relevant question
    current_q <- questions_loop[V2 == q]
    
    ## Merge on dataframe information for q
    current_q <- merge(current_q, df, by.x="V1", by.y="variable")
    
    ## Remove empty rows
    current_q <- current_q[!is.na(value)]
    
    ## Re-create table to be wide structure instead of very long
    if(nrow(current_q) > 0){
      ## Reshape to wide x ~ y where x will be rows, y columns 
      current_q <- dcast(
        current_q, 
        qcountry+respondent_id+wave+V4 ~ V5+V6+V7+V8, 
        value.var="value"
      )
      
      ## Order table by respondent_id wave and the row (V4) 
      class(current_q$V4) <- "integer"
      setorder(current_q, qcountry, respondent_id, wave, V4)
      colnames(current_q)[which(colnames(current_q) == "V4")] <- "table_row"
      
      ## Remove NA's in column names
      colnames(current_q) <- sapply(
        strsplit(colnames(current_q), "_"),
        function(x){
          paste0(x[which(x != "NA")], collapse="_")
        }
      )
      ## Assign current_q to object table_q
      assign(paste0("table_",q), current_q, envir = .GlobalEnv) 
    } else {
      message(paste0("table for ", q, " is empty"))
      # store empty_tables
      empty_tables <- c(empty_tables, q)
      
    }
  }
  
  ## Create a data table of the "loop" variable questions
  ## Sapply creates a list of loop variables names by splitting where "_" occurs
  questions_list_scale <- sapply(
    df[respondent_id==1 & grepl("scale", variable) & 
         !grepl("loop", variable), variable], 
    strsplit, split="_"
  )
  
  ## Lapply creates a data.table by looping over the sapply and puting the 
  ## Different parts of the varibale names in different columns of the new
  ## data table
  questions_scale <- rbindlist(
    lapply(questions_lists,
           function(x){ as.data.table(t(c(paste0(x,collapse="_"),x))) }
    ),
    fill=TRUE
  )
  
  
  #these are not really tables
  questions_scale <- questions_scale[!is.na(V5)]
  questions_scale[, newname := paste0(V2,"_",V5)]
  questions_scale[, "tablename" := paste0("table_",V2)]
  for(q in unique(questions_scale[, V2])){
    
    current_q <- questions_scale[V2 == q]
    
    current_q <- merge(current_q, df, by.x="V1", by.y="variable")
    
    ## Remove empty rows
    current_q <- current_q[!is.na(value)]
    
    ## Re-create table
    if(nrow(current_q) > 0){
      #current_q <- dcast(current_q, qcountry+respondent_id+wave+V3 ~ V2+V5, value.var="value")
      current_q <- dcast(current_q, qcountry+respondent_id+wave+V4 ~ V5+V6+V7, value.var="value")
    
      ## Order table in correct rows
      class(current_q$V3) <- "integer"
      setorder(current_q, qcountry, respondent_id, wave, V3)
      colnames(current_q)[which(colnames(current_q) == "V3")] <- "table_row"
      ## Remove NA's in column names
      colnames(current_q) <- sapply(
        strsplit(colnames(current_q), "_"),
        function(x){
          paste0(x[which(x != "NA")], collapse="_")
        }
      )
      #assign in global environment
      assign(paste0("table_",q), current_q) 
    } else {
      message(paste0("table for ", q, " is empty"))
      empty_tables <- c(empty_tables, q)
    }
  }
  
  
  assign("empty_tables", empty_tables, envir = .GlobalEnv) 
  
  ## Remove tables from main dataset and export to global env
  df <- df[!variable %in% c(questions_loop$V1, questions_scale$V1)]
  df <- dcast(df, qcountry+respondent_id+wave ~ variable)
  assign("df", df, envir = .GlobalEnv) 
  
  if(export_var_names){
    questions_update <- rbindlist(
      list(
        questions_loop[, c("V1","newname","tablename")],
        questions_scale[, c("V1","newname","tablename")] 
      )
    )
    #get list of final variable names
    df_codes[variable %in% c(questions_loop$V1, questions_scale$V1), type := "table_variable"]
    
    df_codes[!variable %in% c(questions_loop$V1, questions_scale$V1), type := "single_variable"]
    
    df_codes <- merge(df_codes[respondent_id==1], 
                      questions_update, 
                      by.x="variable", 
                      by.y="V1", 
                      all.x=T)[, -c("value","respondent_id","wave")]
    
    #remove duplicate variables (rows are removed)
    z <- df_codes[type=="table_variable"]
    setorder(z, newname)
    z <- z[c(T, z[2:nrow(z), newname] != z[2:nrow(z)-1, newname])]
    y <- df_codes[type=="single_variable"]
    y[, ipsos_varname := variable]
    z[, ipsos_varname := variable]
    z[, variable := newname]
    df_codes_unique <- rbindlist(
      list(
        y,
        z
      )
    )
    df_codes_unique <- df_codes_unique[,c("variable","ipsos_varname","type","tablename")]
    df_codes_unique <- rbindlist(
      list(
        df_codes_unique,
        data.table(
          variable=unique(df_codes_unique[type == "table_variable", tablename]),
          ipsos_varname=rep(NA, length(unique(df_codes_unique[type == "table_variable", tablename]))),
          type=rep("table", length(unique(df_codes_unique[type == "table_variable", tablename]))),
          tablename=rep(NA, length(unique(df_codes_unique[type == "table_variable", tablename])))
        )
      )
    )
    setorder(df_codes_unique, type, variable)
    #' variable is the name of the variable in the current dataset
    #' 
    #' ipsos_varname is the name ipsos initially named this variable
    #'  some ipsos_varnames has rows embedded in them, these are now removed, and all duplicate values 
    #'  are removed as well. Only the first old varname remains
    #'  
    #' type states the type of variable. Type single_variable is a variable that is found in the main dataset (df),
    #'  and has only a single record per participant and wave
    #'  Type table_variable is a variable that is embedded in a table, usually because there can be more than one record
    #'  per respondent and wave (i.e. householdmembers, contactpersons, etc)
    #'  Type table is a record for a table in which table_variables are stored. these do not have a direct variable associated with them.
    #'  
    #' tablename gives the name of the table where the variable can be found
    fwrite(df_codes_unique, "~/workspace/contact_survey_2020/data/df_codes_cleaning.csv") 
  }
  
}
















