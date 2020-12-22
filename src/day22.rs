#![feature(deque_range)]

use std::collections::HashSet;
use std::collections::VecDeque;

type Deck = VecDeque<i64>;

struct Game {
    player1: Deck,
    player2: Deck,
}

fn score(deck: &Deck) -> i64 {
    let size = deck.len() as i64;
    deck.iter()
        .enumerate()
        .map(|(i, x)| (size - i as i64) * x)
        .sum()
}

impl Game {
    fn new(player1: &Deck, player2: &Deck) -> Self {
        Game {
            player1: player1.clone(),
            player2: player2.clone(),
        }
    }

    fn winning_score(&self) -> i64 {
        if self.player1.is_empty() {
            score(&self.player2)
        } else {
            score(&self.player1)
        }
    }

    fn game_over(&self) -> bool {
        self.player1.is_empty() || self.player2.is_empty()
    }

    fn run_game(&mut self) {
        while !self.game_over() {
            self.step();
        }
    }

    fn step(&mut self) {
        let card1 = self.player1.pop_front().unwrap();
        let card2 = self.player2.pop_front().unwrap();

        if card1 > card2 {
            self.player1.push_back(card1);
            self.player1.push_back(card2);
        } else {
            self.player2.push_back(card2);
            self.player2.push_back(card1);
        }
    }
}

struct RecursiveGame {
    player1: Deck,
    player2: Deck,
}

#[derive(Copy, Clone, Debug)]
enum Player {
    One,
    Two,
}

impl RecursiveGame {
    fn new(player1: &Deck, player2: &Deck) -> Self {
        RecursiveGame {
            player1: player1.clone(),
            player2: player2.clone(),
        }
    }

    fn winning_score(&self) -> i64 {
        if self.player1.is_empty() {
            score(&self.player2)
        } else if self.player2.is_empty() {
            score(&self.player1)
        } else {
            score(&self.player1)
        }
    }

    fn game_over(&self) -> bool {
        self.player1.is_empty() || self.player2.is_empty()
    }

    fn run(&mut self) -> Player {
        let mut seen = HashSet::new();

        let mut turn = 1;
        while !self.game_over() {
            println!("=== Round {} ===", turn);
            turn += 1;

            if seen.contains(&(self.player1.clone(), self.player2.clone())) {
                return Player::One;
            }

            seen.insert((self.player1.clone(), self.player2.clone()));

            self.step();

            println!("");
        }

        if self.player1.is_empty() {
            Player::Two
        } else {
            Player::One
        }
    }

    fn step(&mut self) {
        println!("Player 1's deck: {:?}", self.player1);
        println!("Player 2's deck: {:?}", self.player2);

        let card1 = self.player1.pop_front().unwrap();
        let card2 = self.player2.pop_front().unwrap();

        println!("Player 1 plays: {}", card1);
        println!("Player 2 plays: {}", card2);

        if self.player1.len() as i64 >= card1 && self.player2.len() as i64 >= card2 {
            let player1 = &self.player1.range(..(card1 as usize)).cloned().collect();
            let player2 = &self.player2.range(..(card2 as usize)).cloned().collect();
            let mut sub_game = RecursiveGame::new(&player1, &player2);
            let result = sub_game.run();
            match result {
                Player::One => {
                    self.player1.push_back(card1);
                    self.player1.push_back(card2);
                }
                Player::Two => {
                    self.player2.push_back(card2);
                    self.player2.push_back(card1);
                }
            }
        } else if card1 > card2 {
            self.player1.push_back(card1);
            self.player1.push_back(card2);
        } else {
            self.player2.push_back(card2);
            self.player2.push_back(card1);
        }
    }
}

fn parse_cards(s: &str) -> anyhow::Result<Deck> {
    Ok(s.lines()
        .skip(1)
        .map(|x| x.parse::<i64>().unwrap())
        .collect())
}

fn read_decks(filename: &str) -> anyhow::Result<(Deck, Deck)> {
    let content = std::fs::read_to_string(filename)?;

    let mut sections = content.split("\n\n");

    let player1 = parse_cards(sections.next().unwrap())?;
    let player2 = parse_cards(sections.next().unwrap())?;

    Ok((player1, player2))
}

fn main() -> anyhow::Result<()> {
    let (player1, player2) = read_decks("input/day22")?;
    let mut game = Game::new(&player1, &player2);
    game.run_game();

    println!("Part 1 = {}", game.winning_score());

    let (player1, player2) = read_decks("input/day22")?;
    let mut game2 = RecursiveGame::new(&player1, &player2);
    game2.run();

    println!("Part 2 = {}", game2.winning_score());

    Ok(())
}
