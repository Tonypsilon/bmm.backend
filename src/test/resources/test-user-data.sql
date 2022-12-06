insert into users(username, password, enabled)
values
('admin', '{bcrypt}$2a$10$tJ2R42TOYqBjQDbFVVdbSOOIdNLsOxbQcIhkWHsfpkLy5G2AXiz8a', true);

insert into authorities(username, authority)
values
('admin', 'admin');