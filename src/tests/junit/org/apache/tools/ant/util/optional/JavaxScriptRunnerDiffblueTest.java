package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JavaxScriptRunnerDiffblueTest {
  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), true));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage2() {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage3() {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "15").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage4() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), false));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage5() {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader(new AntClassLoader(), false);
    classLoader.setIsolated(true);

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) Language is {@code en}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage_givenJavaxScriptRunnerLanguageIsEn_thenReturnFalse() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#supportsLanguage()}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) ScriptClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage_givenJavaxScriptRunnerScriptClassLoaderIsAntClassLoader() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader());
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), true));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript2() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript3() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "15").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript4() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), false));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript5() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader(new AntClassLoader(), false);
    classLoader.setIsolated(true);

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) Language is {@code en}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript_givenJavaxScriptRunnerLanguageIsEn_thenThrowBuildException() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#executeScript(String)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) ScriptClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript_givenJavaxScriptRunnerScriptClassLoaderIsAntClassLoader() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader());
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), true));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript2() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript3() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader();
    classLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "15").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript4() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader(new AntClassLoader(), false));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript5() throws BuildException {
    // Arrange
    AntClassLoader classLoader = new AntClassLoader(new AntClassLoader(), false);
    classLoader.setIsolated(true);

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(classLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) Language is {@code en}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript_givenJavaxScriptRunnerLanguageIsEn_thenThrowBuildException() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test {@link JavaxScriptRunner#evaluateScript(String)}.
   * <ul>
   *   <li>Given {@link JavaxScriptRunner} (default constructor) ScriptClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript_givenJavaxScriptRunnerScriptClassLoaderIsAntClassLoader() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader());
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JavaxScriptRunner}
   *   <li>{@link JavaxScriptRunner#getManagerName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JavaxScriptRunner actualJavaxScriptRunner = new JavaxScriptRunner();
    String actualManagerName = actualJavaxScriptRunner.getManagerName();

    // Assert
    assertEquals("", actualJavaxScriptRunner.getScript());
    assertEquals("javax", actualManagerName);
    assertNull(actualJavaxScriptRunner.getLanguage());
    assertNull(actualJavaxScriptRunner.getProject());
    assertFalse(actualJavaxScriptRunner.getCompiled());
    assertFalse(actualJavaxScriptRunner.getKeepEngine());
  }
}
