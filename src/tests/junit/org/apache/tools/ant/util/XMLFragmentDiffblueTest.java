package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class XMLFragmentDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link XMLFragment}
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    XMLFragment actualXmlFragment = new XMLFragment();

    // Assert
    assertNull(actualXmlFragment.getDescription());
    assertNull(actualXmlFragment.getProject());
    Location location = actualXmlFragment.getLocation();
    assertEquals(0, location.getLineNumber());
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
  }

  /**
   * Method under test: default or parameterless constructor of {@link XMLFragment}
   */
  @Test
  public void testConstructor2() {
    // Arrange and Act
    XMLFragment actualXmlFragment = new XMLFragment();

    // Assert
    assertNull(actualXmlFragment.getDescription());
    assertNull(actualXmlFragment.getProject());
  }

  /**
   * Method under test: {@link XMLFragment#createDynamicElement(String, String, String)}
   */
  @Test
  public void testCreateDynamicElement() {
    // Arrange, Act and Assert
    assertTrue((new XMLFragment()).createDynamicElement("foo", "foo", "foo") instanceof XMLFragment.Child);
    assertTrue((new XMLFragment()).createDynamicElement("", "Name", "Q Name") instanceof XMLFragment.Child);
  }
}

