CREATE TABLE users(
    user_id serial PRIMARY KEY,
    name text NOT NULL,
    surname text NOT NULL, 
    avatar text, 
    date_created timestamp NOT NULL,
    is_admin boolean DEFAULT false
    );

CREATE TABLE authors(
    author_id serial PRIMARY KEY,
    user_id integer NOT NULL REFERENCES users ON DELETE CASCADE,
    description text
    );

CREATE TABLE tags(
    tag_id serial PRIMARY KEY,
    name text
    );

CREATE TABLE categories(
    category_id serial PRIMARY KEY,
    name text,
    parent_id integer REFERENCES categories ON DELETE SET NULL
);

CREATE TABLE news(
    news_id serial PRIMARY KEY,
    title text NOT NULL,
    date_created timestamp NOT NULL,
    author_id integer REFERENCES authors ON DELETE CASCADE,
    category_id integer REFERENCES categories ON DELETE CASCADE,
    content text,
    main_photo text,
    is_draft boolean DEFAULT true
);

CREATE TABLE tags_news(
    tag_id integer REFERENCES tags ON DELETE CASCADE,
    news_id integer REFERENCES news ON DELETE CASCADE,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE commentaries(
    commentary_id serial PRIMARY KEY,
    content text NOT NULL,
    news_id integer REFERENCES news
);
