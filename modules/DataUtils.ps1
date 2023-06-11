###################################################################
#
# Utility module for dealing with data files like CSV and JSON
#
###################################################################
@{
    #H Convert a CSV string into objects
    FromCSV = {param ($object) [tinylist]::new(($object | ConvertFrom-csv))}

    #H Convert objects into a CSV string
    ToCSV = {param ($object) $object | ConvertTo-CSV}

    #H Import data from a CSV file
    ImportCsv = {param ($path) [tinylist]::new((Import-CSV $path)) }

    #H Convert objects into a JSON string
    ToJSON = {param ($object) $object | ConvertTo-JSON -Depth 10}

    #H Convert a JSON string into objects
    FromJSON = {param ($object) [TinyList]::new(($object | ConvertFrom-JSON)) }

    #H Import data from .csv, .json or .xml files
    ImportData = {param ($fileToImport)
            if (-not (Test-Path $fileToImport)) {
                errorMessage "Import-Data: can't find file '$fileToImport'"
            }
            switch -regex ($fileToImport) {
            '\.csv$' { return [TinyList]::new(($fileToImport | ConvertFrom-JSON)) }
            '\.json$' { 
                    $obj = Get-Content -Raw $fileToImport
                    $obj | ConvertFrom-JSON
            }
            '\.xml$' {
                    return [xml] (Get-Content -Raw $fileToImport)
            }
            default {
                errorMessage "ImportData: unsupported format; only csv, json and xml are supported"
            }
        }
    }
}
