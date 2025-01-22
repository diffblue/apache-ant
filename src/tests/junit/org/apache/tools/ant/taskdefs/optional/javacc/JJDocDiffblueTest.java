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

public class JJDocDiffblueTest {
  /**
   * Test new {@link JJDoc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JJDoc}
   */
  @Test
  public void testNewJJDoc() {
    // Arrange and Act
    JJDoc actualJjDoc = new JJDoc();

    // Assert
    Location location = actualJjDoc.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJjDoc.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJjDoc.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJjDoc.getTaskName());
    assertNull(actualJjDoc.getTaskType());
    assertNull(actualJjDoc.getProject());
    assertNull(actualJjDoc.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJjDoc, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link JJDoc#execute()}.
   * <p>
   * Method under test: {@link JJDoc#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JJDoc jjDoc = new JJDoc();
    jjDoc.setTarget(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jjDoc.execute());
  }

  /**
   * Test {@link JJDoc#execute()}.
   * <ul>
   *   <li>Given {@link JJDoc} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JJDoc#execute()}
   */
  @Test
  public void testExecute_givenJJDoc() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JJDoc()).execute());
  }

  /**
   * Test {@link JJDoc#execute()}.
   * <ul>
   *   <li>Given {@link JJDoc} (default constructor) Text is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JJDoc#execute()}
   */
  @Test
  public void testExecute_givenJJDocTextIsTrue() throws BuildException {
    // Arrange
    JJDoc jjDoc = new JJDoc();
    jjDoc.setText(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> jjDoc.execute());
  }
}
