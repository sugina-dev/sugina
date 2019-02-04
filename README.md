# Sugina

_The main back end for the Sugina Development Group website_

## Design

This project provides the following interfaces for the website:

* **Authorization**: Support GitLab login by OAuth application
* **User Management**: Identify users by a unique number
* **Random Dictum**: Randomly choose a dictum in Old Japanese
* **Kunyomi**: Find the Kunyomi of Kanji
* **Message board**: Enable users to post comments and administrators to reply

The program runs on port 3000. It is designed to be used with a reverse proxy.

## Configuration

Before build, the configuration file, `.secret.json`, should be set up correctly, according to the definition in `lib/Sugina/Secret.hs`.

To support GitLab login, there should be an OAuth application on GitLab, and the callback URL should be https://example.com/api/auth/page/gitlab/callback.

## Build and Run

* **Prerequisite**: [Haskell Stack](https://www.haskellstack.org/)
* **Build** (First time): `stack build && stack runhaskell -- -Wall -Werror util/Config.hs`
* **Build** (Many times): `stack build`
* **Run**: `stack exec -- sugina`

## Project Structure

* `lib/`: modules which are used by other modules
* `src/`: source codes to compiled into binaries
* `util/`: scripts for the premiere of the server

## APIs

All the APIs described below are designed to be used with the `/api/` prefix (i.e. `/dictum` becomes https://example.com/api/dictum).

The APIs are defined in `src/Handler/`.

The authorization process is defined in `src/Foundation.hs`. Unauthorized users would receive status code 401.

### **GET** Methods

| URI | Path Parameters | Return Type | Return Values | Authorization |
| :- | :- | :- | :- | :- |
| `/isuser` | / | text/plain | user name | User |
| `/isadmin` | / | text/plain | / | Administrator |
| `/users` | / | application/json | array | Administrator |
| `/dictum` | / | text/plain | random dictum | / |
| `/kunyomi` | q: ch | application/json | an array of all the yomikata | / |
| `/hanja` | q: ch | application/json | an array of all the yomikata | / |
| `/board/message` | / | application/json | all messages sent by current user | User |
| `/board/manage` | / | application/json | all previous messages | Administrator |

### **POST** Methods

| URI | Parameters | Return Values | Authorization |
| :- | :- | :- | :- |
| `/board/message` | json String | / | User |
| `/board/manage` | `{ boardId: Int, reply: String }` | / | Administrator |
