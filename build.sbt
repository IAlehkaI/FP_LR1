ThisBuild / scalaVersion := "3.3.1" // Использована стабильная LTS версия Scala 3

lazy val root = (project in file("."))
  .settings(
    name := "LR1",
    version := "0.1.0-SNAPSHOT",
    // Запускаем в отдельном процессе для чистоты
    run / fork := true,
    // Соединяем ввод консоли с запущенным процессом (ИСПРАВЛЕНИЕ)
    run / connectInput := true,
    // Устанавливаем кодировку для JVM
    run / javaOptions += "-Dfile.encoding=UTF-8"
  )
