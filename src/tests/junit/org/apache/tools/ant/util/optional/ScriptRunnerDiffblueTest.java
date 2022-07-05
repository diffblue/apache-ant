package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import junit.runner.TestCaseClassLoader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.CommonsLoggingListener;
import org.junit.Test;

public class ScriptRunnerDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link ScriptRunner}
  *   <li>{@link ScriptRunner#getManagerName()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("bsf", (new ScriptRunner()).getManagerName());
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript2() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript3() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("Key", null);
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript4() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(new TestCaseClassLoader());
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript5() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("self", "Bean");
    scriptRunner.addBean("Key", null);
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript6() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(new AntClassLoader());
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript7() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript8() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript9() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript10() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new TestCaseClassLoader());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript11() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new TestCaseClassLoader());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript2() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript3() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("Key", null);
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript4() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(new TestCaseClassLoader());
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript5() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.addBean("self", "Bean");
    scriptRunner.addBean("Key", null);
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript6() throws BuildException {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(new AntClassLoader());
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript7() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript8() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript9() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript10() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new TestCaseClassLoader());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript11() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new TestCaseClassLoader());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    antClassLoader.addPathElement(".");

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setScriptClassLoader(antClassLoader);
    scriptRunner.addBean("Key", "Bean");
    scriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link ScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage() {
    // Arrange
    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setProject(new Project());
    scriptRunner.setLanguage("eng");

    // Act and Assert
    assertFalse(scriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link ScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setProject(project);
    scriptRunner.setLanguage("eng");

    // Act and Assert
    assertFalse(scriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link ScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage3() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition(".", Object.class);
    project.addBuildListener(new AntClassLoader());

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setProject(project);
    scriptRunner.setLanguage("eng");

    // Act and Assert
    assertFalse(scriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link ScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage4() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setProject(project);
    scriptRunner.setLanguage("eng");

    // Act and Assert
    assertFalse(scriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link ScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage5() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new CommonsLoggingListener());

    ScriptRunner scriptRunner = new ScriptRunner();
    scriptRunner.setProject(project);
    scriptRunner.setLanguage("eng");

    // Act and Assert
    assertFalse(scriptRunner.supportsLanguage());
  }
}

