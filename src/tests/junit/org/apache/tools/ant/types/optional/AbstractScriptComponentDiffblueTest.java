package org.apache.tools.ant.types.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util.ScriptRunnerBase;
import org.apache.tools.ant.util.optional.JavaxScriptRunner;
import org.junit.Test;

public class AbstractScriptComponentDiffblueTest {
  /**
   * Test {@link AbstractScriptComponent#setProject(Project)}.
   * <p>
   * Method under test: {@link AbstractScriptComponent#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    ScriptCondition scriptCondition = new ScriptCondition();
    Project project = new Project();

    // Act
    scriptCondition.setProject(project);

    // Assert
    assertSame(project, scriptCondition.getProject());
  }

  /**
   * Test {@link AbstractScriptComponent#getRunner()}.
   * <ul>
   *   <li>Then return Project Targets {@code .class} is {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractScriptComponent#getRunner()}
   */
  @Test
  public void testGetRunner_thenReturnProjectTargetsClassIsTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget(".class", target);
    project.addBuildListener(new AntClassLoader());

    ScriptCondition scriptCondition = new ScriptCondition();
    scriptCondition.setProject(project);
    scriptCondition.addText(".class");
    scriptCondition.setLanguage("en");

    // Act
    ScriptRunnerBase actualRunner = scriptCondition.getRunner();

    // Assert
    assertTrue(actualRunner instanceof JavaxScriptRunner);
    Project project2 = actualRunner.getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertEquals(1, project2.getBuildListeners().size());
    assertSame(target, targets.get(".class"));
    assertSame(target, copyOfTargets.get(".class"));
  }

  /**
   * Test {@link AbstractScriptComponent#getRunner()}.
   * <ul>
   *   <li>Then return Project Targets empty string is {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractScriptComponent#getRunner()}
   */
  @Test
  public void testGetRunner_thenReturnProjectTargetsEmptyStringIsTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget("", target);
    project.addBuildListener(new AntClassLoader());

    ScriptCondition scriptCondition = new ScriptCondition();
    scriptCondition.setProject(project);
    scriptCondition.addText(".class");
    scriptCondition.setLanguage("en");

    // Act
    ScriptRunnerBase actualRunner = scriptCondition.getRunner();

    // Assert
    assertTrue(actualRunner instanceof JavaxScriptRunner);
    Project project2 = actualRunner.getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertEquals(1, project2.getBuildListeners().size());
    assertSame(target, targets.get(""));
    assertSame(target, copyOfTargets.get(""));
  }

  /**
   * Test {@link AbstractScriptComponent#createClasspath()}.
   * <ul>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractScriptComponent#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnDescriptionIsNull() {
    // Arrange
    ScriptCondition scriptCondition = new ScriptCondition();
    Project project = new Project();
    scriptCondition.setProject(project);

    // Act
    Path actualCreateClasspathResult = scriptCondition.createClasspath();

    // Assert
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertSame(project, actualCreateClasspathResult.getProject());
  }
}
