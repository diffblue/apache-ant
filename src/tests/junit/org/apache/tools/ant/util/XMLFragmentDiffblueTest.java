package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.util.XMLFragment.Child;
import org.junit.Test;

public class XMLFragmentDiffblueTest {
  /**
   * Test new {@link XMLFragment} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XMLFragment}
   */
  @Test
  public void testNewXMLFragment() {
    // Arrange and Act
    XMLFragment actualXmlFragment = new XMLFragment();

    // Assert
    Location location = actualXmlFragment.getLocation();
    assertNull(location.getFileName());
    assertNull(actualXmlFragment.getDescription());
    assertNull(actualXmlFragment.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link XMLFragment#createDynamicElement(String, String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@link Child}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLFragment#createDynamicElement(String, String, String)}
   */
  @Test
  public void testCreateDynamicElement_whenEmptyString_thenReturnChild() {
    // Arrange, Act and Assert
    assertTrue((new XMLFragment()).createDynamicElement("", "Name", "Q Name") instanceof Child);
  }
}
