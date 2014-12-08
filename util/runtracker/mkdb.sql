CREATE TABLE tests
(
    id INTEGER PRIMARY KEY,
    name TEXT,
    commitHash TEXT
);

CREATE TABLE configs
(
    id INTEGER PRIMARY KEY,
    name TEXT,
    commitHash TEXT
);

CREATE TABLE machines
(
    id INTEGER PRIMARY KEY,
    name TEXT
);

CREATE TABLE runs
(
    id INTEGER PRIMARY KEY,
    
    cmdLine TEXT,
    date    TEXT,
    threads INTEGER,

    testid    INTEGER,
    configid  INTEGER,
    statsid   INTEGER,
    machineid INTEGER,

    FOREIGN KEY(testid)     REFERENCES tests(id),
    FOREIGN KEY(configid)   REFERENCES configs(id),
    FOREIGN KEY(statsid)    REFERENCES stats(id),
    FOREIGN KEY(machineid)  REFERENCES machines(id)
);

CREATE TABLE stats
(
    id INTEGER PRIMARY KEY,

    runtime     REAL,
    testRuntime REAL
);

CREATE TABLE statsSTM
(
    id INTEGER PRIMARY KEY,
    runid INTEGER,

    cap          INTEGER,
    starts       INTEGER,
    aborts       INTEGER,
    failedWakeup INTEGER,
    
    FOREIGN KEY(runid) REFERENCES runs(id)
);

CREATE TABLE statsHTM
(
    id INTEGER PRIMARY KEY,
    runid INTEGER,

    starts   INTEGER,
    capacity INTEGER,
    conflict INTEGER,

    FOREIGN KEY(runid) REFERENCES runs(id)
);
