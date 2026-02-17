function formatSelectedFiles(pathString) {
    if (!pathString) return ""
    const filenames = pathString.split(";").map(path => {
        // Replace "  " with "&nbsp;&nbsp;" for non-breaking spaces
        // Replace "\n" with "<br>" for HTML line breaks
        return "&nbsp;&nbsp;" + path.split("/").pop() + "<br>"
    })
    return filenames.join("")
}