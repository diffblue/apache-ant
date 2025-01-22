package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.apache.tools.ant.taskdefs.EchoXML.NamespacePolicy;
import org.apache.tools.ant.util.DOMElementWriter;
import org.apache.tools.ant.util.DOMElementWriter.XmlNamespacePolicy;
import org.junit.Test;

public class EchoXMLDiffblueTest {
  /**
   * Test {@link EchoXML#execute()}.
   * <p>
   * Method under test: {@link EchoXML#execute()}
   */
  @Test
  public void testExecute() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new EchoXML()).execute());
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#getPolicy()}.
   * <ul>
   *   <li>Given {@link NamespacePolicy#NamespacePolicy()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NamespacePolicy#getPolicy()}
   */
  @Test
  public void testNamespacePolicyGetPolicy_givenNamespacePolicy_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new NamespacePolicy()).getPolicy());
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#getPolicy()}.
   * <ul>
   *   <li>Then return {@link DOMElementWriter.XmlNamespacePolicy#IGNORE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NamespacePolicy#getPolicy()}
   */
  @Test
  public void testNamespacePolicyGetPolicy_thenReturnIgnore() {
    // Arrange and Act
    XmlNamespacePolicy actualPolicy = (new NamespacePolicy(OnError.POLICY_IGNORE)).getPolicy();

    // Assert
    assertSame(actualPolicy.IGNORE, actualPolicy);
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#getPolicy()}.
   * <ul>
   *   <li>Then return {@link DOMElementWriter.XmlNamespacePolicy#ONLY_QUALIFY_ELEMENTS}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NamespacePolicy#getPolicy()}
   */
  @Test
  public void testNamespacePolicyGetPolicy_thenReturnOnly_qualify_elements() {
    // Arrange and Act
    XmlNamespacePolicy actualPolicy = (new NamespacePolicy("elementsOnly")).getPolicy();

    // Assert
    assertSame(actualPolicy.ONLY_QUALIFY_ELEMENTS, actualPolicy);
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#getPolicy()}.
   * <ul>
   *   <li>Then return {@link DOMElementWriter.XmlNamespacePolicy#QUALIFY_ALL}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NamespacePolicy#getPolicy()}
   */
  @Test
  public void testNamespacePolicyGetPolicy_thenReturnQualify_all() {
    // Arrange and Act
    XmlNamespacePolicy actualPolicy = (new NamespacePolicy("all")).getPolicy();

    // Assert
    assertSame(actualPolicy.QUALIFY_ALL, actualPolicy);
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#getValues()}.
   * <p>
   * Method under test: {@link NamespacePolicy#getValues()}
   */
  @Test
  public void testNamespacePolicyGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{OnError.POLICY_IGNORE, "elementsOnly", "all"},
        (new NamespacePolicy("all")).getValues());
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#NamespacePolicy()}.
   * <p>
   * Method under test: {@link NamespacePolicy#NamespacePolicy()}
   */
  @Test
  public void testNamespacePolicyNewNamespacePolicy() {
    // Arrange and Act
    NamespacePolicy actualNamespacePolicy = new NamespacePolicy();

    // Assert
    assertNull(actualNamespacePolicy.getValue());
    assertEquals(-1, actualNamespacePolicy.getIndex());
  }

  /**
   * Test NamespacePolicy {@link NamespacePolicy#NamespacePolicy(String)}.
   * <ul>
   *   <li>When {@link OnError#POLICY_IGNORE}.</li>
   *   <li>Then return Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link NamespacePolicy#NamespacePolicy(String)}
   */
  @Test
  public void testNamespacePolicyNewNamespacePolicy_whenPolicy_ignore_thenReturnIndexIsZero() {
    // Arrange and Act
    NamespacePolicy actualNamespacePolicy = new NamespacePolicy(OnError.POLICY_IGNORE);

    // Assert
    assertEquals(0, actualNamespacePolicy.getIndex());
    assertEquals(OnError.POLICY_IGNORE, actualNamespacePolicy.getValue());
    assertArrayEquals(new String[]{OnError.POLICY_IGNORE, "elementsOnly", "all"}, actualNamespacePolicy.getValues());
  }

  /**
   * Test new {@link EchoXML} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link EchoXML}
   */
  @Test
  public void testNewEchoXML() {
    // Arrange and Act
    EchoXML actualEchoXML = new EchoXML();

    // Assert
    Location location = actualEchoXML.getLocation();
    assertNull(location.getFileName());
    assertNull(actualEchoXML.getDescription());
    assertNull(actualEchoXML.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
