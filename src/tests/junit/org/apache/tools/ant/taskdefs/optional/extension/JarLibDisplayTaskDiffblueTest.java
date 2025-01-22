package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class JarLibDisplayTaskDiffblueTest {
  /**
   * Test {@link JarLibDisplayTask#execute()}.
   * <p>
   * Method under test: {@link JarLibDisplayTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JarLibDisplayTask jarLibDisplayTask = new JarLibDisplayTask();
    jarLibDisplayTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jarLibDisplayTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibDisplayTask.execute());
  }

  /**
   * Test {@link JarLibDisplayTask#execute()}.
   * <p>
   * Method under test: {@link JarLibDisplayTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    JarLibDisplayTask jarLibDisplayTask = new JarLibDisplayTask();
    jarLibDisplayTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "'%s' is not a file.").toFile());
    jarLibDisplayTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibDisplayTask.execute());
  }

  /**
   * Test {@link JarLibDisplayTask#execute()}.
   * <ul>
   *   <li>Given {@link JarLibDisplayTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibDisplayTask#execute()}
   */
  @Test
  public void testExecute_givenJarLibDisplayTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JarLibDisplayTask()).execute());
  }

  /**
   * Test new {@link JarLibDisplayTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JarLibDisplayTask}
   */
  @Test
  public void testNewJarLibDisplayTask() {
    // Arrange and Act
    JarLibDisplayTask actualJarLibDisplayTask = new JarLibDisplayTask();

    // Assert
    Location location = actualJarLibDisplayTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJarLibDisplayTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJarLibDisplayTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJarLibDisplayTask.getTaskName());
    assertNull(actualJarLibDisplayTask.getTaskType());
    assertNull(actualJarLibDisplayTask.getProject());
    assertNull(actualJarLibDisplayTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJarLibDisplayTask, runtimeConfigurableWrapper.getProxy());
  }
}
