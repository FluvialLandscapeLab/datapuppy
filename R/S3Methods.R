print.dpSet = function(x, indent = "") {
  for(idx in 1:length(x)) {
    if(is.list(x[[idx]])) {
      cat(indent, names(x[idx]), ": \n", sep = "")
      print.dpSet(x[[idx]], paste0(indent, "  "))
    } else if(isS4(x[[idx]])) {
        cat(indent, names(x[idx]),": <", class(x[[idx]]), ">\n", sep = "")
    } else {
      cat(indent, names(x[idx]), ": ", paste0(x[[idx]], collapse = ", "), "\n", sep = "")
    }
  }
}

