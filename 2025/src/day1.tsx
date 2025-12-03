import example from "./examples/1.txt?raw";
import input from "./inputs/1.txt?raw";

function parseCommand(instruction) {
    const direction = instruction.substr(0, 1)
    const distance = Number(instruction.substr(1))
    if (direction === "L") {
        return -1 * distance;
    }
    else if (direction === "R") {
        return distance;
    }
    throw(new Error(`Invalid instruction: ${instruction}`))
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function day1(element, commands) {
    const dial = element.querySelector(".dial")
    const part1 = element.querySelector(".part1")
    const part2 = element.querySelector(".part2")
    const lines = commands.split("\n").filter((item) => item);
    const distances = lines.map(parseCommand);
    let value = 50;
    let password = 0;
    let password2 = 0;
    part1.textContent = password;
    for (const distance of distances) {
        await sleep(3000 / distances.length);
        if (distance > 0) {
            password2 += Math.floor(((((value % 100) + 100) % 100) + distance) / 100);
        } else {
            password2 += -1 * Math.ceil(((((value % 100) - 100) % 100) + distance) / 100);
        }
        part2.textContent = password2;

        value = value + distance;
        console.log(value)
        dial.style.transform = `rotate(${3.6 * value}deg)`;
        if (value % 100 === 0) {
            password += 1;
            part1.textContent = password;
        }
    }
    console.log(password)
}

day1(document.getElementById("example"), example);
day1(document.getElementById("input"), input);
