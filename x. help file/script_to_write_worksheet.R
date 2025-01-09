
## Need data on worksheet to see all headers and footers
writeData(wb, sheet = "OS caba", m_model_parameters_OS_mito,   rowNames = TRUE)
writeData(wb, sheet = "OS mito", m_model_parameters_OS_mito,   rowNames = TRUE)
addWorksheet(wb, "test",  gridLines = FALSE, tabColour = "skyblue")

writeData(wb, sheet = "test", x = l_cholesky,   startCol = 1, rowNames = TRUE )

## Save workbook
## Not run: 
saveWorkbook(wb, "Excel_Sheet_with_results.xlsx", overwrite = TRUE)

## End(Not run)