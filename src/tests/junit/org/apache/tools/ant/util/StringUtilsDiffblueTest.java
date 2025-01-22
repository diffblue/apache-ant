package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class StringUtilsDiffblueTest {
  /**
   * Test {@link StringUtils#lineSplit(String)}.
   * <p>
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
   * Test {@link StringUtils#split(String, int)}.
   * <ul>
   *   <li>When one hundred sixteen.</li>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#split(String, int)}
   */
  @Test
  public void testSplit_whenOneHundredSixteen_thenReturnSizeIsTwo() {
    // Arrange and Act
    Vector<String> actualSplitResult = StringUtils.split("Data", 116);

    // Assert
    assertEquals(2, actualSplitResult.size());
    assertEquals("Da", actualSplitResult.get(0));
    assertEquals("a", actualSplitResult.get(1));
  }

  /**
   * Test {@link StringUtils#split(String, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#split(String, int)}
   */
  @Test
  public void testSplit_whenOne_thenReturnSizeIsOne() {
    // Arrange and Act
    Vector<String> actualSplitResult = StringUtils.split("Data", 1);

    // Assert
    assertEquals(1, actualSplitResult.size());
    assertEquals("Data", actualSplitResult.get(0));
  }

  /**
   * Test {@link StringUtils#replace(String, String, String)}.
   * <p>
   * Method under test: {@link StringUtils#replace(String, String, String)}
   */
  @Test
  public void testReplace() {
    // Arrange, Act and Assert
    assertEquals("Data", StringUtils.replace("Data", "jane.doe@example.org", "alice.liddell@example.org"));
  }

  /**
   * Test {@link StringUtils#endsWith(StringBuffer, String)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link StringBuffer#StringBuffer(String)} with {@code foo} append {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#endsWith(StringBuffer, String)}
   */
  @Test
  public void testEndsWith_givenFoo_whenStringBufferWithFooAppendFoo_thenReturnFalse() {
    // Arrange
    StringBuffer buffer = new StringBuffer("foo");
    buffer.append("foo");

    // Act and Assert
    assertFalse(StringUtils.endsWith(buffer, "Suffix"));
  }

  /**
   * Test {@link StringUtils#endsWith(StringBuffer, String)}.
   * <ul>
   *   <li>When {@link StringBuffer#StringBuffer(String)} with {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#endsWith(StringBuffer, String)}
   */
  @Test
  public void testEndsWith_whenStringBufferWithFoo_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(StringUtils.endsWith(new StringBuffer("foo"), "Suffix"));
  }

  /**
   * Test {@link StringUtils#resolveBackSlash(String)}.
   * <p>
   * Method under test: {@link StringUtils#resolveBackSlash(String)}
   */
  @Test
  public void testResolveBackSlash() {
    // Arrange, Act and Assert
    assertEquals("Input", StringUtils.resolveBackSlash("Input"));
  }

  /**
   * Test {@link StringUtils#parseHumanSizes(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#parseHumanSizes(String)}
   */
  @Test
  public void testParseHumanSizes_when42_thenReturnFortyTwo() throws Exception {
    // Arrange, Act and Assert
    assertEquals(42L, StringUtils.parseHumanSizes("42"));
  }

  /**
   * Test {@link StringUtils#parseHumanSizes(String)}.
   * <ul>
   *   <li>When {@code Human Size}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#parseHumanSizes(String)}
   */
  @Test
  public void testParseHumanSizes_whenHumanSize_thenThrowBuildException() throws Exception {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> StringUtils.parseHumanSizes("Human Size"));
  }

  /**
   * Test {@link StringUtils#removeSuffix(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#removeSuffix(String, String)}
   */
  @Test
  public void testRemoveSuffix_whenEmptyString() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removeSuffix("String", ""));
  }

  /**
   * Test {@link StringUtils#removeSuffix(String, String)}.
   * <ul>
   *   <li>When {@code Suffix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#removeSuffix(String, String)}
   */
  @Test
  public void testRemoveSuffix_whenSuffix() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removeSuffix("String", "Suffix"));
  }

  /**
   * Test {@link StringUtils#removePrefix(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#removePrefix(String, String)}
   */
  @Test
  public void testRemovePrefix_whenEmptyString() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removePrefix("String", ""));
  }

  /**
   * Test {@link StringUtils#removePrefix(String, String)}.
   * <ul>
   *   <li>When {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#removePrefix(String, String)}
   */
  @Test
  public void testRemovePrefix_whenPrefix() {
    // Arrange, Act and Assert
    assertEquals("String", StringUtils.removePrefix("String", "Prefix"));
  }

  /**
   * Test {@link StringUtils#join(Object[], CharSequence)} with {@code array}, {@code separator}.
   * <ul>
   *   <li>When array of {@link Object} with {@code Array}.</li>
   *   <li>Then return {@code Array}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoinWithArraySeparator_whenArrayOfObjectWithArray_thenReturnArray() {
    // Arrange, Act and Assert
    assertEquals("Array", StringUtils.join(new Object[]{"Array"}, null));
  }

  /**
   * Test {@link StringUtils#join(Object[], CharSequence)} with {@code array}, {@code separator}.
   * <ul>
   *   <li>When {@link StringUtils#LINE_SEP}.</li>
   *   <li>Then return {@code Array}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoinWithArraySeparator_whenLine_sep_thenReturnArray() {
    // Arrange, Act and Assert
    assertEquals("Array", StringUtils.join(new Object[]{"Array"}, StringUtils.LINE_SEP));
  }

  /**
   * Test {@link StringUtils#join(Object[], CharSequence)} with {@code array}, {@code separator}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Object[], CharSequence)}
   */
  @Test
  public void testJoinWithArraySeparator_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join((Object[]) null, null));
  }

  /**
   * Test {@link StringUtils#join(Collection, CharSequence)} with {@code collection}, {@code separator}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoinWithCollectionSeparator_given42_whenArrayListAdd42_thenReturn42() {
    // Arrange
    ArrayList<Object> collection = new ArrayList<>();
    collection.add("42");

    // Act and Assert
    assertEquals("42", StringUtils.join(collection, StringUtils.LINE_SEP));
  }

  /**
   * Test {@link StringUtils#join(Collection, CharSequence)} with {@code collection}, {@code separator}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code 42 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoinWithCollectionSeparator_given42_whenArrayListAdd42_thenReturn4242() {
    // Arrange
    ArrayList<Object> collection = new ArrayList<>();
    collection.add("42");
    collection.add("42");

    // Act and Assert
    assertEquals("42\n42", StringUtils.join(collection, StringUtils.LINE_SEP));
  }

  /**
   * Test {@link StringUtils#join(Collection, CharSequence)} with {@code collection}, {@code separator}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoinWithCollectionSeparator_whenArrayList_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join(new ArrayList<>(), StringUtils.LINE_SEP));
  }

  /**
   * Test {@link StringUtils#join(Collection, CharSequence)} with {@code collection}, {@code separator}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoinWithCollectionSeparator_whenArrayList_thenReturnEmptyString2() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join(new ArrayList<>(), null));
  }

  /**
   * Test {@link StringUtils#join(Collection, CharSequence)} with {@code collection}, {@code separator}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#join(Collection, CharSequence)}
   */
  @Test
  public void testJoinWithCollectionSeparator_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", StringUtils.join((Collection<?>) null, null));
  }

  /**
   * Test {@link StringUtils#trimToNull(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#trimToNull(String)}
   */
  @Test
  public void testTrimToNull_whenEmptyString_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(StringUtils.trimToNull(""));
  }

  /**
   * Test {@link StringUtils#trimToNull(String)}.
   * <ul>
   *   <li>When {@code Input String}.</li>
   *   <li>Then return {@code Input String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#trimToNull(String)}
   */
  @Test
  public void testTrimToNull_whenInputString_thenReturnInputString() {
    // Arrange, Act and Assert
    assertEquals("Input String", StringUtils.trimToNull("Input String"));
  }

  /**
   * Test {@link StringUtils#trimToNull(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringUtils#trimToNull(String)}
   */
  @Test
  public void testTrimToNull_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(StringUtils.trimToNull(null));
  }
}
