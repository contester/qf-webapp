-- Network printers that print agents report (contester.printers). Apply by hand with psql.
CREATE TABLE agent_printers (
    agent   text        NOT NULL,
    unc     text        NOT NULL,
    server  text        NOT NULL,
    share   text        NOT NULL,
    seen    timestamptz NOT NULL,
    CONSTRAINT agent_printers_pkey PRIMARY KEY (agent, unc)
);
