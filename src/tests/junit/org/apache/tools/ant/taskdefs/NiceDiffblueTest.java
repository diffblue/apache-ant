package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class NiceDiffblueTest {
  /**
   * Test {@link Nice#setNewPriority(int)}.
   * <ul>
   *   <li>When eleven.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Nice#setNewPriority(int)}
   */
  @Test
  public void testSetNewPriority_whenEleven_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Nice()).setNewPriority(11));
  }

  /**
   * Test {@link Nice#setNewPriority(int)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Nice#setNewPriority(int)}
   */
  @Test
  public void testSetNewPriority_whenZero_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Nice()).setNewPriority(0));
  }

  /**
   * Test new {@link Nice} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Nice}
   */
  @Test
  public void testNewNice() {
    // Arrange and Act
    Nice actualNice = new Nice();

    // Assert
    Location location = actualNice.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNice.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualNice.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualNice.getTaskName());
    assertNull(actualNice.getTaskType());
    assertNull(actualNice.getProject());
    assertNull(actualNice.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualNice, runtimeConfigurableWrapper.getProxy());
  }
}
