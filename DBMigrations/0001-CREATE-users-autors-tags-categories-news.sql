-- TODO: NOT NULL, DEFAULT, ON DELETE, ON UPDATE

CREATE TABLE users(
    user_id serial PRIMARY KEY,
    name text NOT NULL,
    surname text NOT NULL, 
    avatar text, 
    date_created timestamp, -- NOT NULL
    is_admin boolean DEFAULT false
    );

CREATE TABLE authors(
    author_id serial PRIMARY KEY,
    user_id integer REFERENCES users NOT NULL,
    description text
    );

CREATE TABLE tags(
    tag_id serial PRIMARY KEY,
    name text
    );

CREATE TABLE categories(
    category_id serial PRIMARY KEY,
    name text,
    parent_id integer REFERENCES categories
);

CREATE TABLE news(
    news_id serial PRIMARY KEY,
    title text,
    date_created timestamp,
    author_id integer REFERENCES authors,
    category_id integer REFERENCES categories,
    content text,
    main_photo text,
    is_draft boolean
);

CREATE TABLE tags_news(
    tag_id integer REFERENCES tags,
    news_id integer REFERENCES news,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE commentaries(
    commentary_id serial PRIMARY KEY,
    content text NOT NULL,
    news_id integer REFERENCES news
);