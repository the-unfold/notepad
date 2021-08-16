CREATE TABLE IF NOT EXISTS events (
    "event_id" serial NOT NULL PRIMARY KEY,
    "ts" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "uuid" uuid NOT NULL UNIQUE,
    "body" jsonb NOT NULL
);

CREATE TABLE IF NOT EXISTS users (
    "user_id" serial NOT NULL PRIMARY KEY,
    "email" text NOT NULL
);

CREATE TABLE IF NOT EXISTS notes (
    "note_id" serial NOT NULL PRIMARY KEY,
    "user_id" INTEGER REFERENCES users("user_id") NOT NULL ,
    "content" text NOT NULL
);

CREATE TABLE IF NOT EXISTS last_processed_event (
    "event_id" INTEGER REFERENCES events("event_id") NOT NULL
);

CREATE INDEX "notes_userid_idx" ON notes ("user_id");

COMMENT ON TABLE events IS 'Events';

COMMENT ON COLUMN events.event_id IS 'Event id (we use it to resume event consuming)';

COMMENT ON COLUMN events.ts IS 'Event registration time';

COMMENT ON COLUMN events.uuid IS 'UUID is for idempotency. TODO: try the uuid + client timestamp approach';

COMMENT ON COLUMN events.body IS 'Event JSON body';

