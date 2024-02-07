# Script to clean up ./GP/src directory. Only keep functions that are used

is_function = function (expr) {
    if (! is_assign(expr)) return(FALSE)
    value = expr[[3L]]
    is.call(value) && as.character(value[[1L]]) == 'function'
}

function_name = function (expr) {
    as.character(expr[[2L]])
}

is_assign = function (expr) {
    is.call(expr) && as.character(expr[[1L]]) %in% c('=', '<-', 'assign')
}

get_functions_list <- function(filename) {
file_parsed = parse(filename)
functions = Filter(is_function, file_parsed)
unlist(Map(function_name, functions))
}

filename = "./GP/src/PowerLawUtils.R"
all_functions = get_functions_list(filename)

# list of all scripts and Rmd files
all_files <- list.files(pattern=".Rmd", recursive=TRUE, full.names=TRUE)

# loop over files, look for functions
df <- data.frame(File=character(), Function=character(), count=integer())
i <- 0
for(filename in all_files) {
    lines <- readLines(filename)
    for(f.name in all_functions) {
        f.match <- grep(f.name, lines)
        if(length(f.match) > 0) {
            i <- i+1
            df[i,] <- c(filename, f.name, length(f.match))
        }
    }
}

print(df)