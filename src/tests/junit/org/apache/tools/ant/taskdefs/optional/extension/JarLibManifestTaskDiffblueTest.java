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
import org.junit.Test;

public class JarLibManifestTaskDiffblueTest {
  /**
   * Test {@link JarLibManifestTask#execute()}.
   * <p>
   * Method under test: {@link JarLibManifestTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JarLibManifestTask jarLibManifestTask = new JarLibManifestTask();
    jarLibManifestTask.setDestfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jarLibManifestTask.execute());
  }

  /**
   * Test {@link JarLibManifestTask#execute()}.
   * <ul>
   *   <li>Given {@link JarLibManifestTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarLibManifestTask#execute()}
   */
  @Test
  public void testExecute_givenJarLibManifestTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JarLibManifestTask()).execute());
  }

  /**
   * Test new {@link JarLibManifestTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JarLibManifestTask}
   */
  @Test
  public void testNewJarLibManifestTask() {
    // Arrange and Act
    JarLibManifestTask actualJarLibManifestTask = new JarLibManifestTask();

    // Assert
    Location location = actualJarLibManifestTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJarLibManifestTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJarLibManifestTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJarLibManifestTask.getTaskName());
    assertNull(actualJarLibManifestTask.getTaskType());
    assertNull(actualJarLibManifestTask.getProject());
    assertNull(actualJarLibManifestTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJarLibManifestTask, runtimeConfigurableWrapper.getProxy());
  }
}
