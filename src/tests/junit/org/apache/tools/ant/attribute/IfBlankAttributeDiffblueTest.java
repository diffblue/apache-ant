package org.apache.tools.ant.attribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.attribute.IfBlankAttribute.Unless;
import org.junit.Test;

public class IfBlankAttributeDiffblueTest {
  /**
   * Test {@link IfBlankAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfBlankAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_when42_thenReturnFalse() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    // Act and Assert
    assertFalse(ifBlankAttribute.isEnabled(new UnknownElement("Element Name"), "42"));
  }

  /**
   * Test {@link IfBlankAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfBlankAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_whenEmptyString_thenReturnTrue() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    // Act and Assert
    assertTrue(ifBlankAttribute.isEnabled(new UnknownElement("Element Name"), ""));
  }

  /**
   * Test {@link IfBlankAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfBlankAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_whenNull_thenReturnTrue() {
    // Arrange
    IfBlankAttribute ifBlankAttribute = new IfBlankAttribute();

    // Act and Assert
    assertTrue(ifBlankAttribute.isEnabled(new UnknownElement("Element Name"), null));
  }

  /**
   * Test new {@link IfBlankAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IfBlankAttribute}
   */
  @Test
  public void testNewIfBlankAttribute() {
    // Arrange and Act
    IfBlankAttribute actualIfBlankAttribute = new IfBlankAttribute();

    // Assert
    Location location = actualIfBlankAttribute.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIfBlankAttribute.getDescription());
    assertNull(actualIfBlankAttribute.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualIfBlankAttribute.isPositive());
  }

  /**
   * Test Unless new {@link Unless} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Unless}
   */
  @Test
  public void testUnlessNewUnless() {
    // Arrange and Act
    Unless actualUnless = new Unless();

    // Assert
    Location location = actualUnless.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnless.getDescription());
    assertNull(actualUnless.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualUnless.isPositive());
  }
}
