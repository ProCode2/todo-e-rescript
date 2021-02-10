/*
Sample JS implementation of Todo CLI that you can attempt to port:
https://gist.github.com/jasim/99c7b54431c64c0502cfe6f677512a87
*/

/* Returns date with the format: 2021-02-04 */
let getToday: unit => string = %raw(`
function() {
  let date = new Date();
  return date.toISOString().slice(0, 10);
}
  `)
type fsConfig = {encoding: string, flag: string}

/* https://nodejs.org/api/fs.html#fs_fs_existssync_path */
@bs.module("fs") external existsSync: string => bool = "existsSync"

/* https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options */
@bs.module("fs")
external readFileSync: (string, fsConfig) => string = "readFileSync"

/* https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options */
@bs.module("fs")
external appendFileSync: (string, string, fsConfig) => unit = "appendFileSync"

@bs.module("fs")
external writeFileSync: (string, string, fsConfig) => unit = "writeFileSync"

/* https://nodejs.org/api/os.html#os_os_eol */
@bs.module("os") external eol: string = "EOL"

@bs.scope("process") @bs.val external argv: array<string> = "argv"

@bs.scope("process") @bs.val external cwd: unit => string = "cwd"

let encoding = "utf8"
let pending_todos_file = cwd() ++ "/todo.txt"
let completed_todos_file = cwd() ++ "/done.txt"
let help_text = `Usage :-
$ ./todo add "todo item"  # Add a new todo
$ ./todo ls               # Show remaining todos
$ ./todo del NUMBER       # Delete a todo
$ ./todo done NUMBER      # Complete a todo
$ ./todo help             # Show usage
$ ./todo report           # Statistics`

// variant of all the valid options
type command =
  | Help
  | Ls
  | Add(option<string>)
  | Del(option<int>)
  | Done(option<int>)
  | Report

// convert the command to its specific type
let parseCommand = (~cmnd: string, ~arg: option<string>): command => {
  let cmnd = cmnd->Js.String.trim->Js.String.toLocaleLowerCase
  // convert to integer
  let pos = arg->Belt.Option.flatMap(str => str->Belt.Int.fromString)
  switch cmnd {
  | "help" => Help
  | "ls" => Ls
  | "add" => Add(arg)
  | "del" => Del(pos)
  | "done" => Done(pos)
  | "report" => Report
  | _ => Help
  }
}

// return array of todos from a given file
let readFile = (filename: string): array<string> => {
  if !existsSync(filename) {
    []
  } else {
    let text = readFileSync(filename, {encoding: encoding, flag: "r"})
    let lines = Js.String.split(eol, text)
    let lines = Js.Array.filter(todo => todo !== "", lines)
    lines
  }
}

// write to file
let writeFile = (filename: string, lines: array<string>) => {
  // if only one element in the array
  if Belt.Array.length(lines) == 1 {
    let text = lines[0] ++ eol
    writeFileSync(filename, text, {encoding: encoding, flag: "w"})
  } else {
    let text = Belt.Array.joinWith(lines, eol, x => x)
    writeFileSync(filename, text, {encoding: encoding, flag: "w"})
  }
}

// write in the next line
let appendFile = (filename: string, content: string) => {
  appendFileSync(filename, content ++ eol, {encoding: encoding, flag: "a"})
}

// update todos
let updateFile = (filename: string, updateFn: array<string> => array<string>) => {
  let todos = readFile(filename)

  let new_todos = updateFn(todos)
  writeFile(pending_todos_file, new_todos)
}

let printHelp = () => {
  Js.log(help_text)
}

// ls
let showRemainingTodos = () => {
  // get all todos
  let todos = readFile(pending_todos_file)

  if Belt.Array.length(todos) == 0 {
    Js.log("There are no pending todos!")
  } else {
    todos
    ->Belt.Array.reverse
    ->Belt.Array.reduceWithIndex("", (acc, todo, index) =>
      acc ++ `[${(todos->Js.Array.length - index)->Belt.Int.toString}] ${todo}${eol}`
    )
    ->Js.log
  }
}

// add a todo
let addTodo = (todo: option<string>) => {
  switch todo {
  | Some(todo) =>
    appendFile(pending_todos_file, todo)
    Js.log(`Added todo: "${todo}"`)

  | None => Js.log("Error: Missing todo string. Nothing added!")
  }
}

// delete todo
let delTodo = (arg: option<int>) => {
  switch arg {
  | Some(number) =>
    // if file exists
    if existsSync(pending_todos_file) {
      updateFile(pending_todos_file, todos => {
        if number < 1 || number > Belt.Array.length(todos) {
          Js.log(`Error: todo #${Belt.Int.toString(number)} does not exist. Nothing deleted.`)
          todos
        } else {
          let todos = Js.Array.filteri((_, index) => index + 1 != number, todos)
          Js.log(`Deleted todo #${Belt.Int.toString(number)}`)
          todos
        }
      })
    }
  | None => Js.log("Error: Missing NUMBER for deleting todo.")
  }
}

// mark a todo as done
let markDone = (arg: option<int>) => {
  switch arg {
  | Some(number) =>
    let todos = readFile(pending_todos_file)
    if number < 1 || number > Belt.Array.length(todos) {
      Js.log(`Error: todo #${Belt.Int.toString(number)} does not exist. Nothing Marked as done.`)
    } else {
      let completedTodo = todos[number - 1]
      // get a new todos array without the todo to be deleted
      let todos = Js.Array.filteri((_, index) => index != number - 1, todos)

      // write pending todos
      writeFile(pending_todos_file, todos)
      // write completed todos
      appendFile(completed_todos_file, `x ${getToday()} ${completedTodo}`)
      Js.log(`Marked todo #${Belt.Int.toString(number)} as done.`)
    }

  | None => Js.log("Error: Missing NUMBER for marking todo as done.")
  }
}

// report
let reportOfTodos = () => {
  let pendingTodos = readFile(pending_todos_file)->Belt.Array.length
  let completedTodos = readFile(completed_todos_file)->Belt.Array.length

  Js.log(
    `${getToday()} Pending : ${Belt.Int.toString(pendingTodos)} Completed : ${Belt.Int.toString(
        completedTodos,
      )}`,
  )
}

// driver code
let cmnd = argv->Belt.Array.get(2)->Belt.Option.getWithDefault("help")
let cmdArg = argv->Belt.Array.get(3)
// typed command
let cmnd: command = parseCommand(~cmnd, ~arg=cmdArg)

switch cmnd {
| Help => printHelp()
| Ls => showRemainingTodos()
| Add(todo) => addTodo(todo)
| Del(pos) => delTodo(pos)
| Done(pos) => markDone(pos)
| Report => reportOfTodos()
}
