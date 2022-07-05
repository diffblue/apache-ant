package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import COM.ibm.netrexx.process.RxProxyLoader;
import COM.ibm.netrexx.process.RxTranslator;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Paths;
import junit.runner.TestCaseClassLoader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class JavaxScriptRunnerDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link JavaxScriptRunner}
  *   <li>{@link JavaxScriptRunner#getManagerName()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("javax", (new JavaxScriptRunner()).getManagerName());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript2() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(new Project());
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript3() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript4() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("%s.%s.%d.%d", Object.class);
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#evaluateScript(String)}
   */
  @Test
  public void testEvaluateScript5() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("%s.%s.%d.%d", new Target());
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.evaluateScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript2() throws BuildException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(new Project());
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript3() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript4() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("%s.%s.%d.%d", Object.class);
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#executeScript(String)}
   */
  @Test
  public void testExecuteScript5() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("%s.%s.%d.%d", new Target());
    project.addBuildListener(new AntClassLoader());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setProject(project);
    javaxScriptRunner.setCompiled(true);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertThrows(BuildException.class, () -> javaxScriptRunner.executeScript("Exec Name"));
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage2() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new RxProxyLoader(new RxTranslator()));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage3() throws MalformedURLException {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(
        new URLClassLoader(new URL[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()}));
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage4() {
    // Arrange
    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(new AntClassLoader());
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage5() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(antClassLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage6() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathElement(".");

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(antClassLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage7() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(antClassLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }

  /**
   * Method under test: {@link JavaxScriptRunner#supportsLanguage()}
   */
  @Test
  public void testSupportsLanguage8() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new TestCaseClassLoader());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JavaxScriptRunner javaxScriptRunner = new JavaxScriptRunner();
    javaxScriptRunner.setScriptClassLoader(antClassLoader);
    javaxScriptRunner.setLanguage("en");

    // Act and Assert
    assertFalse(javaxScriptRunner.supportsLanguage());
  }
}

