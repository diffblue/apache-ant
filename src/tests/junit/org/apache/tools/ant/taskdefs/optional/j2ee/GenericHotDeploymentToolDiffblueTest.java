package org.apache.tools.ant.taskdefs.optional.j2ee;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Java;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.CommandlineJava;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class GenericHotDeploymentToolDiffblueTest {
  /**
   * Test {@link GenericHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@link HotDeploymentTool#ACTION_DEPLOY}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_deploy_thenReturnTrue() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(task);

    // Act and Assert
    assertTrue(genericHotDeploymentTool.isActionValid());
  }

  /**
   * Test {@link GenericHotDeploymentTool#isActionValid()}.
   * <ul>
   *   <li>Given {@link ServerDeploy} (default constructor) Action is {@code Action}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericHotDeploymentTool#isActionValid()}
   */
  @Test
  public void testIsActionValid_givenServerDeployActionIsAction_thenReturnFalse() {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction("Action");

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setTask(task);

    // Act and Assert
    assertFalse(genericHotDeploymentTool.isActionValid());
  }

  /**
   * Test {@link GenericHotDeploymentTool#setTask(ServerDeploy)}.
   * <ul>
   *   <li>When {@link ServerDeploy} (default constructor).</li>
   *   <li>Then {@link GenericHotDeploymentTool} (default constructor) Java Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericHotDeploymentTool#setTask(ServerDeploy)}
   */
  @Test
  public void testSetTask_whenServerDeploy_thenGenericHotDeploymentToolJavaDescriptionIsNull() {
    // Arrange
    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    ServerDeploy task = new ServerDeploy();

    // Act
    genericHotDeploymentTool.setTask(task);

    // Assert
    Java java = genericHotDeploymentTool.getJava();
    assertNull(java.getDescription());
    assertNull(java.getTaskName());
    assertNull(java.getTaskType());
    assertNull(java.getProject());
    assertNull(java.getOwningTarget());
    assertSame(task, genericHotDeploymentTool.getTask());
  }

  /**
   * Test {@link GenericHotDeploymentTool#validateAttributes()}.
   * <p>
   * Method under test: {@link GenericHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setClassName(HotDeploymentTool.ACTION_DEPLOY);
    genericHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    genericHotDeploymentTool.setTask(task);

    // Act
    genericHotDeploymentTool.validateAttributes();

    // Assert that nothing has changed
    CommandlineJava commandLine = genericHotDeploymentTool.getJava().getCommandLine();
    Commandline javaCommand = commandLine.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    Commandline vmCommand = commandLine.getVmCommand();
    assertEquals(0, vmCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, vmCommand.size());
    assertEquals(1, commandLine.size());
    assertEquals(1, vmCommand.getCommandline().length);
    assertEquals(1, commandLine.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
    assertFalse(vmCommand.iterator().hasNext());
    assertEquals(HotDeploymentTool.ACTION_DEPLOY, genericHotDeploymentTool.getClassName());
  }

  /**
   * Test {@link GenericHotDeploymentTool#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GenericHotDeploymentTool#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException() throws BuildException {
    // Arrange
    ServerDeploy task = new ServerDeploy();
    task.setAction(HotDeploymentTool.ACTION_DEPLOY);

    GenericHotDeploymentTool genericHotDeploymentTool = new GenericHotDeploymentTool();
    genericHotDeploymentTool.setClasspath(Path.systemBootClasspath);
    genericHotDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> genericHotDeploymentTool.validateAttributes());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link GenericHotDeploymentTool}
   *   <li>{@link GenericHotDeploymentTool#setClassName(String)}
   *   <li>{@link GenericHotDeploymentTool#getClassName()}
   *   <li>{@link GenericHotDeploymentTool#getJava()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    GenericHotDeploymentTool actualGenericHotDeploymentTool = new GenericHotDeploymentTool();
    actualGenericHotDeploymentTool.setClassName("Class Name");
    String actualClassName = actualGenericHotDeploymentTool.getClassName();
    Java actualJava = actualGenericHotDeploymentTool.getJava();

    // Assert
    assertEquals("Class Name", actualClassName);
    assertNull(actualGenericHotDeploymentTool.getPassword());
    assertNull(actualGenericHotDeploymentTool.getServer());
    assertNull(actualGenericHotDeploymentTool.getUserName());
    assertNull(actualJava);
    assertNull(actualGenericHotDeploymentTool.getTask());
    assertNull(actualGenericHotDeploymentTool.getClasspath());
  }
}
