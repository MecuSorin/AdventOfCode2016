// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing
open System

MSBuildDefaults.Verbosity = Some(Quiet)

// Directories

let buildDir  = "./build/"
let deployDir = "./deploy/"

// define test dlls
let testDlls = !! (buildDir + "/*Tests.dll")

// Filesets
let appReferences  =
    !! "/**/*.*proj"

// version info
let version = "0.1"  // or retrieve from CI server
let cleanDirs () = CleanDirs [buildDir; deployDir]
// Targets
Target "Clean" (fun _ -> cleanDirs() )

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

//  Run tests
let runTest () = 
    testDlls
        |> xUnit2 (fun p -> 
            {p with 
                ShadowCopy = false;
                HtmlOutputPath = Some ("./Tests/" @@ "RunnedTests.html") })

Target "Test" (fun _ -> runTest() )

//   Deploy
Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
        -- "*.zip"
        |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Deploy"

// start build
RunTargetOrDefault "Test"
