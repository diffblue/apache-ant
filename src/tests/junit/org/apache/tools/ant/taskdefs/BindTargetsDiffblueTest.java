package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class BindTargetsDiffblueTest {
  /**
   * Test {@link BindTargets#setOnMissingExtensionPoint(String)} with {@code String}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BindTargets#setOnMissingExtensionPoint(String)}
   */
  @Test
  public void testSetOnMissingExtensionPointWithString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> (new BindTargets()).setOnMissingExtensionPoint("On Missing Extension Point"));
  }

  /**
   * Test {@link BindTargets#execute()}.
   * <ul>
   *   <li>Given {@link BindTargets} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BindTargets#execute()}
   */
  @Test
  public void testExecute_givenBindTargets_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BindTargets()).execute());
  }

  /**
   * Test {@link BindTargets#execute()}.
   * <ul>
   *   <li>Given {@link Target#Target()} Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BindTargets#execute()}
   */
  @Test
  public void testExecute_givenTargetNameIsAttribute_name_thenThrowBuildException() throws BuildException {
    // Arrange
    Target target = new Target();
    target.setName(Manifest.ATTRIBUTE_NAME);

    BindTargets bindTargets = new BindTargets();
    bindTargets.setOwningTarget(target);
    bindTargets.setExtensionPoint("extensionPoint required");

    // Act and Assert
    assertThrows(BuildException.class, () -> bindTargets.execute());
  }

  /**
   * Test {@link BindTargets#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BindTargets#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    BindTargets bindTargets = new BindTargets();
    bindTargets.setExtensionPoint("extensionPoint required");

    // Act and Assert
    assertThrows(BuildException.class, () -> bindTargets.execute());
  }

  /**
   * Test new {@link BindTargets} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BindTargets}
   */
  @Test
  public void testNewBindTargets() {
    // Arrange and Act
    BindTargets actualBindTargets = new BindTargets();

    // Assert
    Location location = actualBindTargets.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBindTargets.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualBindTargets.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualBindTargets.getTaskName());
    assertNull(actualBindTargets.getTaskType());
    assertNull(actualBindTargets.getProject());
    assertNull(actualBindTargets.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualBindTargets, runtimeConfigurableWrapper.getProxy());
  }
}
