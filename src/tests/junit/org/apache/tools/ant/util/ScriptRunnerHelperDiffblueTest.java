package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.apache.tools.ant.util.optional.JavaxScriptRunner;
import org.junit.Test;

public class ScriptRunnerHelperDiffblueTest {
  /**
   * Test {@link ScriptRunnerHelper#getScriptRunner()}.
   * <ul>
   *   <li>Then return Beans {@code project} is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#getScriptRunner()}
   */
  @Test
  public void testGetScriptRunner_thenReturnBeansProjectIsProject() {
    // Arrange
    TaskAdapter component = new TaskAdapter();
    Project project = new Project();
    component.setProject(project);

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(component);

    // Act
    ScriptRunnerBase actualScriptRunner = scriptRunnerHelper.getScriptRunner();

    // Assert
    assertTrue(actualScriptRunner instanceof JavaxScriptRunner);
    Map<String, Object> beans = actualScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertSame(project, beans.get("project"));
    assertSame(project, actualScriptRunner.getProject());
    assertSame(component, beans.get("self"));
  }

  /**
   * Test {@link ScriptRunnerHelper#getScriptRunner()}.
   * <ul>
   *   <li>Then return Project Targets empty string is {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#getScriptRunner()}
   */
  @Test
  public void testGetScriptRunner_thenReturnProjectTargetsEmptyStringIsTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget("", target);

    TaskAdapter component = new TaskAdapter();
    component.setProject(project);

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(component);

    // Act
    ScriptRunnerBase actualScriptRunner = scriptRunnerHelper.getScriptRunner();

    // Assert
    Map<String, Object> beans = actualScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertTrue(beans.get("self") instanceof TaskAdapter);
    assertTrue(actualScriptRunner instanceof JavaxScriptRunner);
    Project project2 = actualScriptRunner.getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertTrue(beans.containsKey("project"));
    assertSame(target, targets.get(""));
    assertSame(target, copyOfTargets.get(""));
  }

  /**
   * Test {@link ScriptRunnerHelper#getScriptRunner()}.
   * <ul>
   *   <li>Then return Project Targets {@code ".} is {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#getScriptRunner()}
   */
  @Test
  public void testGetScriptRunner_thenReturnProjectTargetsQuotationMarkDotIsTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget("\".", target);

    TaskAdapter component = new TaskAdapter();
    component.setProject(project);

    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(component);

    // Act
    ScriptRunnerBase actualScriptRunner = scriptRunnerHelper.getScriptRunner();

    // Assert
    Map<String, Object> beans = actualScriptRunner.getBeans();
    assertEquals(2, beans.size());
    assertTrue(beans.get("self") instanceof TaskAdapter);
    assertTrue(actualScriptRunner instanceof JavaxScriptRunner);
    Project project2 = actualScriptRunner.getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertTrue(beans.containsKey("project"));
    assertSame(target, targets.get("\"."));
    assertSame(target, copyOfTargets.get("\"."));
  }

  /**
   * Test {@link ScriptRunnerHelper#createClasspath()}.
   * <ul>
   *   <li>Given {@link ScriptRunnerHelper} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenScriptRunnerHelper_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new ScriptRunnerHelper()).createClasspath());
  }

  /**
   * Test {@link ScriptRunnerHelper#createClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();
    scriptRunnerHelper.setProjectComponent(new ImplementationSpecificArgument());

    // Act
    Path actualCreateClasspathResult = scriptRunnerHelper.createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link ScriptRunnerHelper#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link ScriptRunnerHelper} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenScriptRunnerHelper_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new ScriptRunnerHelper()).setClasspath(Path.systemBootClasspath));
  }

  /**
   * Test {@link ScriptRunnerHelper#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link ScriptRunnerHelper} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerHelper#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenScriptRunnerHelper_thenThrowIllegalStateException() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> scriptRunnerHelper.setClasspathRef(new Reference("42")));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
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
   *   <li>{@link ScriptRunnerHelper#getSrc()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ScriptRunnerHelper scriptRunnerHelper = new ScriptRunnerHelper();

    // Act
    scriptRunnerHelper.setClassLoader(new AntClassLoader());
    scriptRunnerHelper.setCompiled(true);
    scriptRunnerHelper.setEncoding("UTF-8");
    scriptRunnerHelper.setLanguage("en");
    scriptRunnerHelper.setProjectComponent(Path.systemBootClasspath);
    scriptRunnerHelper.setSetBeans(true);
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    scriptRunnerHelper.setSrc(file);
    scriptRunnerHelper.addText("Text");
    boolean actualCompiled = scriptRunnerHelper.getCompiled();
    String actualEncoding = scriptRunnerHelper.getEncoding();
    String actualLanguage = scriptRunnerHelper.getLanguage();

    // Assert
    assertEquals("UTF-8", actualEncoding);
    assertEquals("en", actualLanguage);
    assertTrue(actualCompiled);
    assertSame(file, scriptRunnerHelper.getSrc());
  }

  /**
   * Test new {@link ScriptRunnerHelper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptRunnerHelper}
   */
  @Test
  public void testNewScriptRunnerHelper() {
    // Arrange and Act
    ScriptRunnerHelper actualScriptRunnerHelper = new ScriptRunnerHelper();

    // Assert
    assertNull(actualScriptRunnerHelper.getSrc());
    assertNull(actualScriptRunnerHelper.getEncoding());
    assertNull(actualScriptRunnerHelper.getLanguage());
    assertFalse(actualScriptRunnerHelper.getCompiled());
  }
}
