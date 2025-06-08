# Life Tracker App

**Life Tracker** is a personal analytics app built to help track and visualize important metrics across key areas of life: faith, relationships, study, strength, and finances. This interactive tool uses a SQLite database and allows users to both enter and visualize data over time.

---

## 🏠 Home Tab

The **Home** page offers a snapshot of your current week and overall life metrics. Key features include:

- **Faith Tracker**: See how many days you've completed your devotion/meditation for the week. Clickable boxes allow you to log days directly, which updates both the database and the **Faith calendar**.
- **Finances**: View your current **Net Assets**, **Net Debt**, and **Net Worth**, all dynamically updated.
- **People to Contact**: Automatically reminds you who to reach out to based on custom intervals and the last date contacted.

---

## 🙏 Faith Tab

The **Faith** section provides insights into your spiritual practices:

- **Calendar View**: A visual representation of days where you’ve logged devotion or meditation time.
- **Raw Table View**: Access raw data from the SQLite table storing your faith practices.
- **Financial Data**: Track and visualize net worth over time with a line chart and table view.
- **Enter Data**: Add new financial entries by date to keep your metrics up to date.

---

## 🤝 Relationships Tab

Manage your social connections with features tailored to meaningful interactions:

- **People Tab**: View all contacts, including name, relationship type, and last contacted date.
- **Notes Tab**: Store detailed notes on past interactions and conversations.
- **Add Person**: Add new contacts into the system with relevant details.
- **Enter Note**: Log a new interaction or follow-up note for a contact.

---

## 📚 Study Tab

Track your educational progress and study habits over time:

- **Home View**: Displays key study metrics like **total minutes**, **total hours**, and your **current program**.
- **Line Chart**: Interactive chart shows time studied over a user-specified date range.
- **View Table**: See raw data entries in table format from the SQLite database.
- **Enter Data**: Log new study time data per day.

---

## 💪 Strength Tab

Keep tabs on your physical development through consistent tracking:

- **Home View**: Shows cumulative totals for **push-ups** and **sit-ups**.
- **Line Charts**: View daily performance over time through separate charts for push-ups and sit-ups.
- **Enter Data**: Easily log your daily strength exercises into the app.

---

## 🛠️ Tech Stack

- **Frontend**: R + Shiny Dashboard  
- **Data Base**: SQLite  
- **Visualization**: Interactive charts and tables  
- **Data Entry**: Form-based updates linked to a persistent local database

---

## 📌 Note

This app was created as a personal tool to enhance self-awareness, reflection, and consistency across the most important areas of life. Future updates may include additional tracking areas, automated reminders, and export options.
