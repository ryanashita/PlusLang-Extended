module Helper

let rec findSlnDirectory (dir: string) : string =
    let slnFiles = System.IO.Directory.GetFiles(dir, "*.sln")
    if slnFiles.Length > 0 then dir
    else
        let parent = System.IO.Directory.GetParent(dir)
        if obj.ReferenceEquals(parent, null) then
            raise (System.Exception(sprintf "Failed to locate .sln file starting from %s" dir))
        else
            findSlnDirectory parent.FullName

let getPath (filename: string) : string =
    let cwd = System.IO.Directory.GetCurrentDirectory()
    let slnDir = findSlnDirectory cwd
    let path = System.IO.Path.Combine(slnDir, "programs", filename)
    if System.IO.File.Exists path then 
        path
    else
        raise (System.IO.FileNotFoundException(sprintf "Test file not found at expected location: %s" path))
