package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import com.ibm.icu.impl.FormattedStringBuilder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class StringUtilsDiffblueTest {
  /**
  * Method under test: {@link StringUtils#endsWith(StringBuffer, String)}
  */
  @Test
  public void testEndsWith() {
    // Arrange, Act and Assert
    assertFalse(StringUtils.endsWith(UnicodeUtil.EscapeUnicode('A'), "Suffix"));
    assertFalse(StringUtils.endsWith(UnicodeUtil.EscapeUnicode('A'), "foo"));
    assertTrue(StringUtils.endsWith(UnicodeUtil.EscapeUnicode('A'), ""));
    assertFalse(StringUtils.endsWith(UnicodeUtil.EscapeUnicode('\u0002'), "42"));
  }

  /**
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoin() {
    // Arrange
    ArrayList<Object> collection = new ArrayList<>();

    // Act and Assert
    assertEquals("", StringUtils.join(collection, new FormattedStringBuilder()));
  }

  /**
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoin2() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join((Collection<?>) null, null));
  }

  /**
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoin3() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join(new ArrayList<>(), null));
  }

  /**
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoin4() {
    // Arrange, Act and Assert
    assertEquals("Array", StringUtils.join(new Object[]{"Array"}, new FormattedStringBuilder()));
  }

  /**
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoin5() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join((Object[]) null, null));
  }

  /**
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoin6() {
    // Arrange, Act and Assert
    assertEquals("Array", StringUtils.join(new Object[]{"Array"}, null));
  }

  /**
   * Method under test: {@link StringUtils#lineSplit(String)}
   */
  @Test
  public void testLineSplit() {
    // Arrange and Act
    Vector<String> actualLineSplitResult = StringUtils.lineSplit("Data");

    // Assert
    assertEquals(1, actualLineSplitResult.size());
    assertEquals("Data", actualLineSplitResult.get(0));
  }

  /**
   * Method under test: {@link StringUtils#parseHumanSizes(String)}
   */
  @Test
  public void testParseHumanSizes() throws Exception {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> StringUtils.parseHumanSizes("Human Size"));
    assertEquals(42L, StringUtils.parseHumanSizes("42"));
  }

  /**
   * Method under test: {@link StringUtils#removePrefix(String, String)}
   */
  @Test
  public void testRemovePrefix() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removePrefix("String", "Prefix"));
    assertEquals("", StringUtils.removePrefix("foo", "foo"));
  }

  /**
   * Method under test: {@link StringUtils#removeSuffix(String, String)}
   */
  @Test
  public void testRemoveSuffix() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removeSuffix("String", "Suffix"));
    assertEquals("", StringUtils.removeSuffix("foo", "foo"));
  }

  /**
   * Method under test: {@link StringUtils#replace(String, String, String)}
   */
  @Test
  public void testReplace() {
    // Arrange, Act and Assert
    assertEquals("Data", StringUtils.replace("Data", "jane.doe@example.org", "alice.liddell@example.org"));
  }

  /**
   * Method under test: {@link StringUtils#resolveBackSlash(String)}
   */
  @Test
  public void testResolveBackSlash() {
    // Arrange, Act and Assert
    assertEquals("Input", StringUtils.resolveBackSlash("Input"));
  }

  /**
   * Method under test: {@link StringUtils#split(String, int)}
   */
  @Test
  public void testSplit() {
    // Arrange and Act
    Vector<String> actualSplitResult = StringUtils.split("Data", 1);

    // Assert
    assertEquals(1, actualSplitResult.size());
    assertEquals("Data", actualSplitResult.get(0));
  }

  /**
   * Method under test: {@link StringUtils#split(String, int)}
   */
  @Test
  public void testSplit2() {
    // Arrange and Act
    Vector<String> actualSplitResult = StringUtils.split("Data", 116);

    // Assert
    assertEquals(2, actualSplitResult.size());
    assertEquals("Da", actualSplitResult.get(0));
    assertEquals("a", actualSplitResult.get(1));
  }

  /**
   * Method under test: {@link StringUtils#trimToNull(String)}
   */
  @Test
  public void testTrimToNull() {
    // Arrange, Act and Assert
    assertEquals("Input String", StringUtils.trimToNull("Input String"));
    assertNull(StringUtils.trimToNull(null));
    assertNull(StringUtils.trimToNull(""));
  }
}

