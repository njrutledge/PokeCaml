# CS-3110-FA19-Project

The project is an open-ended, agile software development experience. Think of it as an activity that takes place instead of a second prelim—and that requires correspondingly great intensity.

There will be four milestones in the project: Charter, Alpha, Beta, and Release. In the Charter phase you will form a team and propose the system you plan to build. In the remaining phases you will build out and demo to your discussion section some part of your system, write a brief report on your progress toward your final goal, and submit your code for evaluation.

Table of Contents:

Deadlines
Project Requirements
Project Ideas
Teams
Managing Conflict
Sprints
Grading
Deadlines
Milestone	Due	Files
MS0	Fri, 10/25/19	Handout
MS1	Wed, 11/06/19	Handout
MS2	Wed, 11/20/19	Handout
MS3	Mon, 12/09/19	Handout
Project Requirements
The primary educational objective of this project is for you to build a system from scratch in OCaml with a team.

“Building from scratch” means that you may not build off of your previous assignments in this class, or current or old assignments in other classes. The work that you do for 3110 needs to be specifically for this course and not another.

“Building from scratch” also means the implementation effort needs to be primarily your own, rather primarily glueing together several third-party libraries. You may use whatever libraries you wish that can be installed from OPAM; no need to ask for our approval. But your project will evaluated based on the code that you write, not on how well you integrated those libraries or how hard they were to use. If you have questions about whether you can use other third-party code or tools, please ask, rather than assuming one way or the other.

“In OCaml” means that only the OCaml code you write will count toward your project. If you happen to do some kind of mixed development where some of the project is implemented in other languages (e.g., Javascript, C, or Reason), that’s fine, but in evaluating your work we’re only going to look at the OCaml portion of it.

“With a team” means that the system you build needs to be a unified, cohesive application that requires all members of the team to work together. It may not be a few similar applications, implemented independently by team members, that are loosely held together. For example, a “casino” in which the user can choose to play three different games is not permitted: it leads to a team building three separate systems that are loosely glued together, instead of collaborating on a single system.

Size: You should aim for a system that has about 500 lines of OCaml code (excluding testing) per team member. That’s not a hard requirement, just an estimate.

Source control: You are required to use Git to manage your source code. Unlike other assignments in this course, you are welcome to make your work on this project public.

Library warning: Neither graphical user interfaces nor networking are required. Be warned that both can be difficult, and no one on the course staff will be able to give you much help with them. So if you want to go for such a project, have a backup plan in case it becomes too hard to get done in time.

Project Ideas
Pick something that you are excited about! Here are some ideas to inspire you:

Game. Create your own version of your favorite game. The game might be two-player (like Battleship) or multi-player (like Texas Hold’em poker). Humans might be able to compete against themselves, another human, or against AIs. Beware that a game in which players act simultaneously in real time could be considerably challenging to implement; we recommend that you instead choose a game in which players take discrete turns. (Of course, because of the “build from scratch” requirement, you may not just extend or re-implement your A2+A3 game. You need to do something completely new.)

Programmable calculator. Build a programmable calculator. Your calculator could have a REPL in which users can enter not just simple arithmetic expressions, but could also write programs in a small language of your design. You could be inspired by the RPL language. You calculator could solve systems of equations or plot functions.

Editor. Build a text editor similar to nano, though likely with a smaller feature set. Your editor would enable creating and changing files on disk. It would support find-and-replace of text. Use an OCaml text interface such as AnsiTerminal to implement the interface.

And here are some harder ideas:

Database. Create a database management system (DBMS) that supports basic SQL queries, including creating and dropping tables; inserting, updating, and deleting rows; and SELECT…FROM…WHERE queries. The database would be stored as a file that is updated throughout execution, so that it survives failures of the server. Your implementation would include a REPL that permits interaction with the DBMS.

Bulletin board. Build an online bulletin board. Piazza, Facebook, and Wikipedia are examples in this genre. Users post content which other users can explore and interact. Users can search for content of interest. Provide a server application, which could be accessed by a web browser. Or perhaps you’d prefer to build a custom client application.

Instant messaging. Build an instant-messaging system. Provide a client application and a server application. Your chat system will enable users to exchange messages that contain text. The server will store messages and deliver them when clients become available. You could be inspired by XMPP or another open-source messaging protocol.

Web browser. Build a text-based web browser similar to lynx. (To install lynx on the VM, run sudo apt install lynx.) Your browser would enable viewing webpages, following hyperlinks, downloading files. It would support searching for text on a page. Use an OCaml text interface such as AnsiTerminal to implement the interface.

Data science. Build libraries similar to Pandas and Matplotlib for OCaml. Don’t just provide OCaml interfaces to those exact libraries; rather, implement table and graphing functionality of your own. You might even be able to use Jupyter notebooks as a front end, if you can get an OCaml kernel working.

Teams
Teams are the norm in industry, building bigger and better software than one person could do alone. Teamwork is a skill that you can develop, just like programming. Now is a good time to develop it. We hope this project gives you an opportunity to do so.

Team formation: Your team must be within your section. You are responsible for forming your own team, but your section TAs will be happy to help match you with others. The MS0 (Charter) assignment has more details about how that will work. The choice of who is on your team is mainly up to you, but the professor does reserve the right to insert and remove people from teams to solve personnel problems (e.g., somebody drops the course, irreconcilable creative differences, etc.).

Team size: Your team must have three members. Two member teams are not permitted. If the enrollment in a section is not a multiple of 3, a couple four-person teams may have to be formed within a section. Only the minimum necessary number of four-person teams will be permitted. Four-person teams are expected to produce larger projects because of their larger team size.

Personnel changes: After the project begins, any changes in team membership must be approved by your section TAs.

Peer ratings: Your team will be asked to establish expectations in writing, and to conduct peer ratings of teamwork—not of technical ability. The peer ratings your team members submit of you will become part of your grade on the project. Specifically, 80% of the project grade will be for the system you build as a team, and all of you will share the same grade for that. But 20% of the project grade will be individual (i.e., could differ for each team member) and determined by those peer ratings.

Managing Conflict
No doubt conflict will arise in solving assignments together. Part of teamwork is managing conflict. We expect you to handle it professionally. The course staff will be available to help you with that. In the worst case, it will be possible for the professor to remove a team member, but not before a serious attempt is made to address the issues at hand by both sides.

How to handle non-cooperative team members:

Step 1. If a team member fails to cooperate and contribute to the the project, the other team members should first meet with that non-cooperative team member to have an honest conversation about how their expectations are not being met.

Step 2. If the problem still continues, the cooperating team members may send an email to the professor, cc’ing the non-cooperative member, to summarize the situation and request a meeting. The professor will read the team’s expectation document (submitted during MS0), then meet with everyone, hoping to resolve the problem. Most of the time, that does succeed.

Step 3. If the problem does not resolve after that meeting, the cooperating team members may warn the uncooperative member by email that they are contemplating asking the professor to remove the non-cooperating member from the team. This email must be written with the utmost professionalism. The professor must be cc’d on that email.

Step 4. If there is still no subsequent improvement, the cooperating team members may send an email to the professor, cc’ing the non-cooperating member, requesting that the non-cooperating member be removed from the team. The professor will make the final call.

A student who is consistently doing all the work for their team without any help from their teammates may follow the same process to quit from their team.

Removal and quitting may result in grade penalties, if they result from failure to be good team citizens. The amount of the penalty will be determined by the professor based on the details of the situation.

Sprints
The project is organized as a sequence of 2-week phases or sprints, during each of which you build some “shippable” increment of your system. That means implementing some functionality that can be demoed to your section and TAs. It’s your choice what that functionality is.

In a successful sprint, the functionality you implement will offer clear value to users of the system—value that can be seen through a running demo of the system, rather than code that isn’t yet runnable or integrated into the system, or whose value can be seen only through unit tests.

Using A2+A3 as an example, a successful sprint would involve building out some new player commands and being able to demo that they work in the interface. An unsuccessful sprint might involve adding JSON fields to the adventure file and adding parsing code for them, but not being able to see the effect of that as a player.

Grading
Of the total percentage dedicated in the syllabus to the final project, we expect the breakdown to be as follows:

MS0 (Charter): 10%
MS1 (Alpha): 10%
MS2 (Beta): 10%
MS3 (Release): 50%
Peer evaluation: 20%
Bonus: 5%
Why is MS3 worth five times the amount of MS1 and MS2? It’s not because you should do five times the work all at the end: it’s because we don’t want any initial stumbles to have too great of an impact. We encourage you to work at a consistent pace throughout the project.

The TAs for each section will, at the end of the project, be able to nominate 20% (rounded to the nearest integer) of the projects in their section for a bonus of 5%. So, the top 1 to 3 projects in a section could receive a bonus. We hope that this results in a small and healthy amount of friendly competition. But, no one should get overly excited about the effect on your final grade: it’s about the same as three days’ lecture attendance.
