package main

import "fmt"
import "encoding/json"

/*
Question:
https://stackoverflow.com/questions/48771861/how-to-get-array-from-json-post-request-and-append-it-to-slice-string-in-golan

Answer:
Assuming your body follows this structure:
{
	"ids": [1, 2, 3]
}

You can put the "ids" array into a slice like this:
*/

type Request struct {
	Ids []int `json:"ids"`
}

func main() {
	body_str := `{ "ids": [1, 2, 3] }`
	var request Request
	err := json.Unmarshal([]byte(body_str), &request)
	if err != nil {
		fmt.Print(err)
	}
	fmt.Printf("%#v", request.Ids)
}
