package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import junit.runner.TestCaseClassLoader;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class ScriptRunnerHelperDiffblueTest {
  /**
   * Method under test: {@link ScriptRunnerHelper#add(ResourceCollection)}
   */
  @Test
  public void testAdd() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.addText("Text");

    // Act
    scriptRunnerHelper.add(null);

    // Assert that nothing has changed
    assertFalse(scriptRunnerHelper.getCompiled());
  }

  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link ScriptRunnerHelper}
  *   <li>{@link ScriptRunnerHelper#setClassLoader(ClassLoader)}
  *   <li>{@link ScriptRunnerHelper#setCompiled(boolean)}
  *   <li>{@link ScriptRunnerHelper#setEncoding(String)}
  *   <li>{@link ScriptRunnerHelper#setLanguage(String)}
  *   <li>{@link ScriptRunnerHelper#setProjectComponent(ProjectComponent)}
  *   <li>{@link ScriptRunnerHelper#setSetBeans(boolean)}
  *   <li>{@link ScriptRunnerHelper#setSrc(File)}
  *   <li>{@link ScriptRunnerHelper#addText(String)}
  *   <li>{@link ScriptRunnerHelper#getCompiled()}
  *   <li>{@link ScriptRunnerHelper#getEncoding()}
  *   <li>{@link ScriptRunnerHelper#getLanguage()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    ScriptRunnerHelper actualScriptRunnerHelper = new ScriptRunnerHelper();
    actualScriptRunnerHelper.setClassLoader(new TestCaseClassLoader());
    actualScriptRunnerHelper.setCompiled(true);
    actualScriptRunnerHelper.setEncoding("UTF-8");
    actualScriptRunnerHelper.setLanguage("en");
    actualScriptRunnerHelper.setProjectComponent(new AntUnit());
    actualScriptRunnerHelper.setSetBeans(true);
    actualScriptRunnerHelper.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualScriptRunnerHelper.addText("Text");

    // Assert
    assertTrue(actualScriptRunnerHelper.getCompiled());
    assertEquals("UTF-8", actualScriptRunnerHelper.getEncoding());
    assertEquals("en", actualScriptRunnerHelper.getLanguage());
  }

  /**
   * Method under test: default or parameterless constructor of {@link ScriptRunnerHelper}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertFalse((new ScriptRunnerHelper()).getCompiled());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#createClasspath()}
   */
  @Test
  public void testCreateClasspath() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new ScriptRunnerHelper()).createClasspath());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#createClasspath()}
   */
  @Test
  public void testCreateClasspath2() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setProjectComponent(new AntUnit());

    // Act
    Path actualCreateClasspathResult = scriptRunnerHelper.createClasspath();

    // Assert
    assertEquals(0, actualCreateClasspathResult.size());
    assertNull(actualCreateClasspathResult.getProject());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#createClasspath()}
   */
  @Test
  public void testCreateClasspath3() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setProjectComponent(antUnit);

    // Act and Assert
    assertEquals(0, scriptRunnerHelper.createClasspath().size());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#getScriptRunner()}
   */
  @Test
  public void testGetScriptRunner() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("org.apache.bsf.BSFManager", Object.class);
    project.addBuildListener(new AntClassLoader());

    AntUnit antUnit = new AntUnit();
    antUnit.setProject(project);

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(antUnit);

    // Act
    ScriptRunnerBase actualScriptRunner = scriptRunnerHelper.getScriptRunner();

    // Assert
    assertEquals(2, actualScriptRunner.getBeans().size());
    assertFalse(actualScriptRunner.getCompiled());
    assertEquals("en", actualScriptRunner.getLanguage());
    assertEquals("", actualScriptRunner.getScript());
    assertSame(project, actualScriptRunner.getProject());
    assertFalse(actualScriptRunner.getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#getScriptRunner()}
   */
  @Test
  public void testGetScriptRunner2() {
    // Arrange
    Project project = new Project();
    project.setDefault("org.apache.bsf.BSFManager");
    project.addBuildListener(new AntClassLoader());

    AntUnit antUnit = new AntUnit();
    antUnit.setProject(project);

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(antUnit);

    // Act
    ScriptRunnerBase actualScriptRunner = scriptRunnerHelper.getScriptRunner();

    // Assert
    assertEquals(2, actualScriptRunner.getBeans().size());
    assertFalse(actualScriptRunner.getCompiled());
    assertEquals("en", actualScriptRunner.getLanguage());
    assertEquals("", actualScriptRunner.getScript());
    assertSame(project, actualScriptRunner.getProject());
    assertFalse(actualScriptRunner.getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> scriptRunnerHelper.setClasspath(new Path(new Project())));
  }

  /**
   * Method under test: {@link ScriptRunnerHelper#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> scriptRunnerHelper.setClasspathRef(new Reference("42")));
  }
}

