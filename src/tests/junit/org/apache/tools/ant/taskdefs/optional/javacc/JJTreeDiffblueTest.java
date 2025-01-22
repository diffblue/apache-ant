package org.apache.tools.ant.taskdefs.optional.javacc;

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

public class JJTreeDiffblueTest {
  /**
   * Test new {@link JJTree} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JJTree}
   */
  @Test
  public void testNewJJTree() {
    // Arrange and Act
    JJTree actualJjTree = new JJTree();

    // Assert
    Location location = actualJjTree.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJjTree.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJjTree.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJjTree.getTaskName());
    assertNull(actualJjTree.getTaskType());
    assertNull(actualJjTree.getProject());
    assertNull(actualJjTree.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJjTree, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link JJTree#execute()}.
   * <p>
   * Method under test: {@link JJTree#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JJTree jjTree = new JJTree();
    jjTree.setTarget(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jjTree.execute());
  }

  /**
   * Test {@link JJTree#execute()}.
   * <ul>
   *   <li>Given {@link JJTree} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JJTree#execute()}
   */
  @Test
  public void testExecute_givenJJTree() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JJTree()).execute());
  }
}
