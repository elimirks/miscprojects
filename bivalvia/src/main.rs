use serde::{Deserialize, Serialize};

// https://platform.openai.com/docs/guides/gpt/chat-completions-api
// https://openai.com/blog/function-calling-and-other-api-updates
//
// https://platform.openai.com/docs/guides/gpt/chat-completions-api (see messages format. Start
// with a system role, then alternate between user and assistant). But can also have a "function"
// role.


// https://json-schema.org/understanding-json-schema/

#[derive(Serialize, Deserialize, Debug, Clone)]
struct AddFunctionArgs {
    nums: Vec<f64>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct OpenAiMessageFunctionCall {
    name: String,
    // Json serialized as a string
    arguments: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct OpenAiMessage {
    role: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    // Required property, but null if it's a function call.
    content: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    function_call: Option<OpenAiMessageFunctionCall>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct OpenAiResponseChoice {
    message: OpenAiMessage,
    finish_reason: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct OpenAiResponse {
    id: String,
    object: String,
    model: String,
    choices: Vec<OpenAiResponseChoice>,
}

fn create_request_body(messages: &[OpenAiMessage]) -> String {
    let mut v: serde_json::Value = serde_json::json!({
        "model": "gpt-3.5-turbo-0613",
        //"model": "gpt-4-0613",
        "messages": [],
        "functions": [
            {
                "name": "add",
                "description": "Adds numbers together. All numbers given are summed up.",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "nums": {
                            "type": "array",
                            "items": {
                                "type": "number"
                            }
                        }
                    },
                }
            },
            {
                "name": "finish",
                "description": "Finishes the conversation. The chat session will terminate after calling this.",
                "parameters": {
                    "type": "object",
                    "properties": {}
                }
            }
        ]
    });
    v["messages"] = serde_json::to_value(messages)
        .expect("OpenAiMessage is unrepresentable as JSON");
    v.to_string()
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    const INTRO_MESSAGE: &str = "Hi! My name is Bivalvia, your friendly assistant.";

    let openai_key = std::env::var("OPENAI_API_KEY")
        .expect("Couldn't find OpenAI key in environment");

    let mut messages = vec![
        OpenAiMessage {
            role: "system".to_owned(),
            name: None,
            content: Some("You're a friendly assistant.".to_owned()),
            function_call: None,
        },
        OpenAiMessage {
            role: "assistant".to_owned(),
            name: None,
            content: Some(INTRO_MESSAGE.to_owned()),
            function_call: None,
        },
    ];

    println!("Bivalvia:\n{}\n", INTRO_MESSAGE);
    let client = reqwest::Client::new();
    let mut should_prompt_user = true;
    loop {
        if should_prompt_user {
            println!("User:");
            let mut line = String::new();
            let _ = std::io::stdin().read_line(&mut line).unwrap();
            println!();
            messages.push(OpenAiMessage {
                role: "user".to_owned(),
                name: None,
                content: Some(line),
                function_call: None,
            });
        }

        let res = client.post("https://api.openai.com/v1/chat/completions")
            .basic_auth("", Some(openai_key.clone()))
            .header("Content-Type", "application/json")
            .body(create_request_body(&messages))
            .send()
            .await?
            .json::<OpenAiResponse>()
            .await?;

        let choice = &res.choices[0];
        messages.push(choice.message.clone());
        if choice.finish_reason == "function_call" {
            // Let the assistant respond to their own function call
            should_prompt_user = false;
            let func = choice.message.function_call
                .as_ref()
                .expect("Finished with a function call but didn't return a function call definition");

            if func.name == "finish" {
                println!("Bivalvia:\n{}\n", "Bye for now!");
                break;
            } else if func.name == "add" {
                let args: AddFunctionArgs = serde_json::from_str(&func.arguments)?;
                let res: f64 = args.nums.iter().sum();
                println!("[debug] calling add({:?}) = {:?}", args, res);
                messages.push(OpenAiMessage {
                    role: "function".to_owned(),
                    name: Some(func.name.clone()),
                    content: Some(format!("{}", res)),
                    function_call: None,
                });
            } else {
                panic!("Unexpected function call: {}. Body: {:?}", func.name, res);
            }
        } else if choice.finish_reason == "stop" {
            let msg = choice.message.content.as_ref()
                .expect("Assistant chat response with no content!")
                .clone();
            println!("Bivalvia:\n{}\n", msg);
            should_prompt_user = true;
        } else {
            // TODO: Handle other finish reasons. There are things like "content length too long"
            // or "content filter".
            panic!("Unexpected response: {:?}", res);
        }
    }
    Ok(())
}

//
// Add these numbers together: 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223
