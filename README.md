# Sugina

_The main back end for the Sugina Development Group website_

## Configuration

Before the premiere of the server program, the configuration file, `.secret.json`, should be set up correctly. Its detailed format is defined in `lib/Sugina/Secret.hs`.

To support GitLab login, the callback URL of the GitLab application should be `<App Root>/api/auth/page/gitlab/callback`.

## Build and Run

* **Prerequisite**: [Haskell Stack](https://www.haskellstack.org/)
* **Build** (Once):
    - Setup `.secret.json` correctly
    - `stack build`
    - `stack runhaskell -- -Wall -Werror utils/emplacedb.hs`
* **Build** (Many Times): `stack build`
* **Run**: `stack exec -- sugina`

## Project Structure

* `lib/`: modules which are used by other modules
* `src/`: source codes to compiled into binaries
* `utils/`: scripts for bootstrap

## APIs

All the APIs should be accessed with the `/api/` prefix.

All the APIs are defined in `src/Handler/`.

The authorization process is defined in `src/Foundation.hs`.

Unauthorized users will receive status code 401.

| URI | Method | Parameters | Return Values | Authorization |
| :- | :- | :- | :- | :- |
| `/isuser` | **GET** | / | json, the user name | User |
| `/isadmin` | **GET** | / | json, `true` | Administrator |
| `/users` | **GET** | / | json array | Administrator |
| `/dictum` | **GET** | / | text | / |
| `/kunyomi` | **GET** | the query word | json, an array of all the yomikata | / |
| `/board/message` | **GET** | / | all messages sent by current user | User |
| `/board/message` | **POST** | json String | json, `true` | User |
| `/board/manage` | **GET** | / | all previous messages | Administrator |
| `/board/manage` | **POST** | `{ boardId: Int, reply: String }` | json, `true` | Administrator |
