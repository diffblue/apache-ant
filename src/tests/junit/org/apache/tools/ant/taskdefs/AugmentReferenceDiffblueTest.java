package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class AugmentReferenceDiffblueTest {
  /**
   * Test {@link AugmentReference#getProxy()}.
   * <ul>
   *   <li>Given {@link AugmentReference} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AugmentReference#getProxy()}
   */
  @Test
  public void testGetProxy_givenAugmentReference_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new AugmentReference()).getProxy());
  }

  /**
   * Test {@link AugmentReference#getProxy()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AugmentReference#getProxy()}
   */
  @Test
  public void testGetProxy_thenThrowBuildException() {
    // Arrange
    AugmentReference augmentReference = new AugmentReference();
    augmentReference.setRuntimeConfigurableWrapper(new RuntimeConfigurable("Proxy", "Project owner unset"));
    augmentReference.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> augmentReference.getProxy());
  }

  /**
   * Test {@link AugmentReference#setProxy(Object)}.
   * <p>
   * Method under test: {@link AugmentReference#setProxy(Object)}
   */
  @Test
  public void testSetProxy() {
    // Arrange, Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new AugmentReference()).setProxy("42"));
  }

  /**
   * Test new {@link AugmentReference} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AugmentReference}
   */
  @Test
  public void testNewAugmentReference() {
    // Arrange and Act
    AugmentReference actualAugmentReference = new AugmentReference();

    // Assert
    Location location = actualAugmentReference.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAugmentReference.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualAugmentReference.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualAugmentReference.getTaskName());
    assertNull(actualAugmentReference.getTaskType());
    assertNull(actualAugmentReference.getProject());
    assertNull(actualAugmentReference.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualAugmentReference, runtimeConfigurableWrapper.getProxy());
  }
}
