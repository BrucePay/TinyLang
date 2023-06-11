
csc /target:library /langversion:7 `
    "/reference:System.Linq.dll,$([psobject].Assembly.Location)" `
    .\LineEditor.cs
