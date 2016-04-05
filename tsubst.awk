{
    if ($0 == "$$TEMPLATE_REPLACED$$") {
        while ((getline line < templatefile) > 0)
            print line
        close(templatefile)
    } else
        print
}
